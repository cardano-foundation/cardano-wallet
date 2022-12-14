## How to prepare a self-hosted runner for this repo

- get your `github-token` from [https://github.com/settings/tokens](https://github.com/settings/tokens), allowing access to the repo and to the workflow (to be assessed)
- place your token in the file system of the host (somewhere safe)
- create space for the `/nix` folder, should go in NIX_VOLUME envvar
- decide a name for your runner, should go in RUNNER_NAME envvar
- fix the makefile to reflect your choices
  
## How to control your runner 

- make deploy, will deploy the runner locally
- make kill, will remove it 
- make logs, will follow the logs
