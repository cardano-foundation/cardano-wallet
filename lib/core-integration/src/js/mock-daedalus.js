#!/usr/bin/env node

// This runs cardano-wallet-launcher in the same way that Daedalus would.
// It needs node, cardano-wallet, and cardano-wallet-launcher on the PATH to run.

const child_process = require("child_process");
const http = require('http');

function main() {
  // Prevent the cardano-wallet child process from writing coverage ".tix" files.
  // Because this test script does not wait for the child process to
  // actually exit, it will linger briefly and interfere with other
  // tests which need to update the coverage files.
  delete process.env.HPCTIXFILE;

  // Spawn the command and arguments supplied on the nodejs command line.
  // NodeJS messaging is enabled with the "ipc" option.
  let args = process.argv.slice(3);
  const proc = child_process.spawn(process.argv[2], args,
    { stdio: ["ignore", "inherit", "inherit", "ipc"] }
  );

  proc.on("close", function(code, signal) {
    console.log("JS: child_process stdio streams closed");
    process.exit(1);
  });

  proc.on("disconnect", function() {
    console.log("JS: child_process disconnected");
    process.exit(2);
  });

  proc.on("error", function(err) {
    console.log("JS: error child_process: " + err);
    process.exit(3);
  });

  proc.on("exit", function(code, signal) {
    console.log("JS: child_process exited with status " + code + " or signal " + signal);
    process.exit(4);
  });

  proc.on("message", function(msg) {
    console.log("JS: message received", msg);
    // See CardanoNode.js in Daedalus for the message types in use.
    if (msg.Started) {
      console.log("JS: sending a bogus message");
      proc.send("hello");
    } else if (msg.ParseError && msg.ParseError.match(/encountered String/)) {
      console.log("JS: sending QueryPort");
      proc.send({ QueryPort: [] });
    } else if (msg.ParseError) {
      console.log("JS: i did not expect that");
      process.exit(5);
    } else if (msg.ReplyPort) {
      http.get({
        hostname: "localhost",
        port: msg.ReplyPort,
        path: "/v2/wallets",
        agent: false
      }, (res) => {
        console.log("JS: response from wallet: " + res.statusCode);
        res.resume();
        res.on("end", () => {
          console.log("JS: request response from wallet finished, exiting.");
          process.exit(0);
          // Exiting the parent process is going to result in
          // cardano-wallet receiving an EOF on the IPC file
          // descriptor, which will cause it to shut down.
        });
      });
    }
  });
}

main();
