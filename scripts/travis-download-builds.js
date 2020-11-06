// This script downloads builds of cardano-wallet from Hydra.
// It is meant for use within the travis release script.

const axios = require('axios');
const _ = require('lodash');
const fs = require('fs');
const path = require('path');

function makeHydraApi(hydraURL, options = {}) {
  const api = axios.create(_.merge({
    baseURL: hydraURL,
    headers: { "Content-Type": "application/json" },
  }, options));
  api.interceptors.request.use(request => {
    console.debug("Hydra " + request.url);
    return request;
  });
  return api;
}

function makeGitHubApi(options = {}) {
  const api = axios.create(_.merge({
    baseURL: "https://api.github.com/",
    headers: { "Content-Type": "application/json" },
  }, options));
  api.interceptors.request.use(request => {
    console.debug(`${request.method} ${request.baseURL}${request.url}`);
    return request;
  });
  return api;
}

async function findEvalByCommit(api, project, jobset, rev, page)  {
  const evalsPath = `jobset/${project}/${jobset}/evals${page || ""}`;
  const r = await api.get(jobPath);

  const eval = _.find(r.data.evals, e => e.jobsetevalinputs["cardano-wallet"].revision === rev);

  if (eval) {
    return eval;
  } else if (r.data.next) {
    return findEvalByCommit(api, project, jobset, rev, r.data.next);
  } else {
    return undefined;
  }
}

function findCardanoWalletEval(api, rev) {
  return findEvalByCommit(apiapi, "Cardano", "cardano-wallet", rev);
}

async function findEvalFromGitHub(hydra, github, owner, repo, ref, page) {
  const q = page ? ("?page=" + page) : "";
  const r = await github.get(`repos/${owner}/${repo}/commits/${ref}/statuses${q}`);

  const status = _.find(r.data, { context: "ci/hydra-eval" });

  if (status) {
    if (status.state === "success") {
      const eval = await hydra.get(status.target_url);
      return eval.data;
    } else if (status.state === "pending") {
       console.log("Eval is pending - trying again...");
       await sleep(1000);
       return await findEvalFromGitHub(hydra, github, owner, repo, ref);
    } else {
      console.error("Can't get eval - it was not successful.");
      return null;
    }
  } else {
    const next = (page || 1) + 1;
    console.log(`Eval not found - trying page ${next}`);
    return await findEvalFromGitHub(hydra, github, owner, repo, ref, next);
  }
}

async function findBuildsInEval(api, eval, jobs) {
  let builds = {};
  for (let i = 0; i < eval.builds.length; i++) {
    const r = await api.get(`build/${eval.builds[i]}`);
    if (_.includes(jobs, r.data.job)) {
      console.log(`Found ${r.data.job}`);
      builds[r.data.job] = r.data;
      if (_.size(builds) === _.size(jobs)) {
        break;
      }
    }
  }
  return builds;
}

async function downloadBuildProduct(outDir, hydraUrl, build, number) {
  const buildProduct = build.buildproducts[number];
  const filename = buildProduct.name;
  const writer = fs.createWriteStream(path.join(outDir, filename));
  const url = `${hydraUrl}build/${build.id}/download/${number}/${filename}`;

  console.log(`Downloading ${url}`);

  await axios({
    method: 'get',
    url,
    responseType: 'stream',
  }).then(response => {
    return new Promise((resolve, reject) => {
      response.data.pipe(writer);
      let error = null;
      writer.on('error', err => {
        error = err;
        writer.close();
        reject(err);
      });
      writer.on('close', () => {
        if (!error) {
          resolve(true);
        }
      });
    });
  });
}

async function download(outDir, downloadSpec, jobs, options = {}) {
  const hydraUrl = "https://hydra.iohk.io/";
  const hydraApi = makeHydraApi(hydraUrl, options);
  const github = makeGitHubApi(options);

  const eval = await findEvalFromGitHub(hydraApi, github, downloadSpec.owner, downloadSpec.repo, downloadSpec.rev);
  console.log(`Eval has ${eval.builds.length} builds`);

  const downloads = downloadSpec.jobs;

  const builds = await findBuildsInEval(hydraApi, eval, _.map(downloads, "job"));

  for (let i = 0; i < downloads.length; i++) {
    const build = builds[downloads[i].job];
    for (let j = 0; j < downloads[i].buildProducts.length; j++) {
      await downloadBuildProduct(outDir, hydraUrl, build, "" + downloads[i].buildProducts[j]);
    }
  }
}

function sleep(ms = 0) {
  return new Promise(r => setTimeout(r, ms));
};

function getDownloadSpec(jobs) {
  const rev = process.env.TRAVIS_COMMIT;
  if (!rev) {
    console.error("The TRAVIS_COMMIT environment variable should be set to a git revision id.");
    process.exit(1);
  }
  return {
    owner: 'input-output-hk',
    repo: 'cardano-wallet',
    rev,
    jobs: _.map(jobs, name => { return { job: name, buildProducts: [1] }; })
  };
}

download(".", getDownloadSpec([
  "cardano-wallet-linux64",
  "cardano-wallet-win64",
  "cardano-wallet-macos64",
]));
