# Logging guidelines

Draft for issue [#351](https://github.com/input-output-hk/cardano-wallet/issues/351)

These logs are primarily for _users_ of the cardano-wallet, not its
developers. Users could be:

- Engineers who will be running the cardano-wallet in production.
- Developers integrating cardano-wallet into their own software.
- Technical support staff who need to help solve end users' problems.

Users of logging will have a reasonable understanding of how the
cardano-wallet works, but won't be familiar with the code. They need
enough information to be able scan the logs and see if the application
is running normally, or if they need to do some action to fix their
problem.

[iohk-monitoring-framework]: https://github.com/input-output-hk/iohk-monitoring-framework

## Logging levels

The [iohk-monitoring-framework][] defines a number of [severity levels](https://github.com/input-output-hk/iohk-monitoring-framework/blob/b0ea8317ba5a887d46969e9b3040862a10e6efb3/iohk-monitoring/src/Cardano/BM/Data/Severity.lhs#L33-L44):


| **Severity Level** | **Meaning** |
| -- | -- |
| Debug     | detailed information about values and decision flow |
| Info      | general information of events; progressing properly |
| Notice    | needs attention; something not progressing properly |
| Warning   | may continue into an error condition if continued |
| Error     | unexpected set of event or condition occurred |
| Critical  | error condition causing degrade of operation |
| Alert     | a subsystem is no longer operating correctly, likely requires manual intervention |
| Emergency | at this point, the system can never progress without additional intervention |

This design was influenced by the [`syslog` severity
level](https://en.wikipedia.org/wiki/Syslog#Severity_level) taxonomy.

These are probably more than we need. To keep things simple, we will
use only the first 5.

| **Severity Level** | **Meaning** |
| -- | -- |
| Debug     | Messages that contain information normally of use only when debugging a program. |
| Info      | Informational messages. |
| Notice    | Normal but significant conditions. |
| Warning   | A situation that may become an error. |
| Error     | Unexpected problem. |

### Debug

Debug logging should never be enabled in production. Therefore, you
should not log information relevant to production operations at this
level.

It might be used to help debug program flow, but aim to remove this
kind of debug logging once features are stable.

It is however very useful to have logging of about how the wallet
interacts with other systems. Examples of DEBUG logs:
- Network requests made to node backend.
- SQL query logs -- log the queries but not the values.

### Info

Normal activity within the application. We do need to ensure that INFO
logs are pertinent, and there is not pages of logs per block.

Examples:
- HTTP requests
- Wallet-related events
  - A block was applied
  - New wallet was created
  - Wallet restoration status
  - Payment was sent
  - Payment was received
- Network-related events
  - Sync state

### Notice

These are occasional messages about significant events for the program.

Examples:
 - The start up message, including the application version.
 - What TCP port the API server is listening on.
 - Normal shut down.
 - Creating a new database file or migrating data.

### Warning

A warning situation could lead to a future error, or other undesirable
behaviour. The user should be able to do something to make the warning
go away.

Examples:
- NTP drift?
- Low disk space?

### Error

This is a serious system problem.
It should not happen under normal circumstances.
Some action is required to rectify the situation.
The wallet might well exit after logging an error message.

Examples:
- Unexpected error communicating with node backend.
- IO error writing database or other state.
- Bad user input which means that the action cannot complete.
- Unhandled exception.

## Mapping of log levels to appearance

### CLI tools

Logging from the CLI should not be cluttered with timestamps and other
metadata. It should just look like a normal print statements in a
program. Nonetheless, using the logging framework for CLI output helps
because error messages can be automatically coloured.

| **Severity** | **Format** |
| -- | -- |
| Debug     | Hidden unless enabled by the user |
| Info      | Printed normally on `stdout` |
| Notice    | Printed in bold on `stdout` |
| Warning   | Printed in bold colour on `stderr` prefixed by `Warning: ` |
| Error     | Printed in bold red on `stderr` prefixed by `ERROR: ` |

### Server

| **Severity** | **Format** |
| -- | -- |
| Debug     | Hidden unless enabled by the user |
| The rest  | As per defaults for [iohk-monitoring-framework][] |

The server will support also support a log config file where the
output location and format can be fully customised.

### Colour

If the log output file is not a terminal, do not output ANSI colour
codes. These escape codes make it difficult to analyse log files.


## Context

Some context may be applicable to log events:

- Current slot number.
- Wallet ID.
- A request ID for the API, so that requests, their handlers, and
  responses can be cross-referenced.

### Logging Hierarchy

Log _Traces_ can have context added such as a new name. Different
subsystems of the wallet (such as network, database, api) should use
`appendName` to create subtraces.

## Observables

The following are examples of things which be easy and useful to log
as micro-benchmarks.

- Time taken for API to respond to request.
- Time taken to execute SQL query.
- Time taken for node backend to respond to request.

## Privacy

[iohk-monitoring-framework][] provides "sensitive" variants of log
functions (e.g. `logInfoS`). These allow sensitive information to be
logged into separate files. Then users may send their logs to the
technical support desk with a reasonable assurance of privacy.

### Public information

 - Wallet ID.
 - Dates and times that events occurred.
 - Total number of addresses/transactions/UTxOs/etc.
 - IP addresses of other nodes that network nodes are communicating
   with.
 
### Private information

 - Transaction ID.
 - Address.
 - Public key material.
 - Quantities of Ada.

### Never log

This information should not be included in log messages (including
DEBUG messages):

 - Private key material.
 - Passphrases.
 - The values used or returned by SQL queries.
 - Raw data sent to or received from the network backend.
 - Raw, un-filtered API request or response bodies.
