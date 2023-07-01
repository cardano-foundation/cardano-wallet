# Logging Guidelines

Logs are primarily for _users_ of the cardano-wallet, not its
developers. Such users could be:

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

These are probably more than we need. To keep things simple, we
usually only log with the first 5:

| **Severity Level** | **Meaning** |
| -- | -- |
| Debug     | Messages that contain information normally of use only when debugging a program. |
| Info      | Informational messages. |
| Notice    | Normal but significant conditions. |
| Warning   | A situation that may become an error. |
| Error     | Unexpected problem. |

### Debug

Debug logging will not be enabled by default in production. Therefore,
you should not log information directly relevant to production
operations at this level.

However, the technical support desk may request users enable debug
logging for certain components to get more detail while trying to
diagnose problems in the field.

Such logging might contain:
- Information about which program decisions were made and why.

Don't debug log too much rubbish because somebody will need to read
it. Don't log so much data that the program will malfunction if debug
logging has been enabled.

It is useful to have debug logging of about how the wallet
interacts with other systems. Examples of such logs:
- Network requests made to node backend.
- SQL query logs -- log the queries but not the values.

Note from Alexander Diemand:
> Another feature that might help reduce logging volume:
> monitoring can watch an observable and compare it to a set
> threshold. Once this is exceeded it can create an action: i.e. an
> alert message or change the severity filter of a named context or
> globally. So one would start up the node with logging filter set to
> "Warning" and only change it to "Info" when some monitored observable
> has exceeded its threshold
>
> For an exchange that runs the node for ages: another monitor could
> also restore the severity filter to e.g. "Warning" if the observed
> values return into a normal behaviour range of values.

### Info

Normal activity within the application. We do need to ensure that INFO
logs are pertinent, and there are not pages and pages of logs per
slot.

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
- Information about the configuration, such as config file location,
  or how the wallet was started.

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

### JSON

JSON (one object per line) is an OK format for log files, but it's
pretty bad for printing the terminal. For example, consider how
difficult it would be to read through JSON messages within `journalctl
-u cardano-wallet.service`. Therefore, log messages to
`stdout`/`stderr` should be formatted as text, unless otherwise
configured by the user.

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

The following are examples of things which should be easy and useful
to log as micro-benchmarks (`bracketObserveIO`).

- Time taken for API to respond to request.
- Time taken to execute SQL query.
- Time taken for node backend to respond to request.

## Privacy

[iohk-monitoring-framework][] provides "sensitive" variants of log
functions (e.g. `logInfoS`). These allow sensitive information to be
logged into separate files. Then users may send their logs to the
technical support desk with a reasonable assurance of privacy.

**Note**: The privacy guidelines are under discussion. We are
considering making it simpler and only having two classifications:
"Log" or "Do Not Log".

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

### Unknown

Undecided about which category these fall into.

 - Wallet name

### Never log

This information should _never_ be included in log messages (including
DEBUG messages):

 - Private key material.
 - Mnemonic sentences.
 - Passphrases.
 - The values used or returned by SQL queries.
 - Raw data sent to or received from the network backend.
 - Raw, un-filtered API request or response bodies.

## Increasing logging detail

[iohk-monitoring-framework][] provides a facility for adjusting log
levels at runtime on a component by component basis.

However it's probably OK for now to change log levels by restarting
cardano-wallet with different command-line options.

## Structured logging tutorial

Rather than logging unstructured text, define a type for log messages
of a module.

```haskell
data FooMsg
    = LogFooInit
    | LogFooError FooError
    | LogFooIncomingEvent Int
    deriving (Show, Eq)
```

Then for human-readable logs use our `ToText` class.

```haskell
import Data.Text.Class
    ( ToText (..) )

instance ToText FooMsg where
    toText msg = case msg of
        LogFooInit -> "The foo has started"
        LogFooError e -> "foo error: " <> T.pack (show e)
        LogFooIncomingEvent -> "Incoming foo event " <>  T.pack (show e)
```

Finally, define the metadata which the switchboard needs to route
these traces.

```haskell
import Cardano.BM.Data.LogItem
    ( LoggerName, PrivacyAnnotation (..) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )

-- Everything is public by default
instance DefinePrivacyAnnotation FooMsg

instance DefineSeverity FooMsg
    defineSeverity msg = case msg of
        LogFooInit -> Debug
        LogFooError _ -> Error
        LogFooIncomingEvent -> Info
```

To use the logging in the `doFoo` function:

```haskell
import Cardano.BM.Trace
    ( Trace )
import Cardano.Wallet.Logging
    ( logTrace )

doFoo :: Trace IO FooMsg -> IO ()
doFoo tr = do
    logTrace tr LogFooInit
    onFooEvent $ \ev -> case ev of
       Right n -> logTrace tr $ LogFooIncomingEvent n
       Left e -> logTrace tr $ LogFooError e
```

To convert to `Trace m FooMsg` to `Trace m Text` (well actually - the
other way around), use `Cardano.Wallet.Logging.transformTextTrace`,
like so:

```haskell
import Control.Tracer
    ( contramap )
import Cardano.Wallet.Logging
    ( transformTextTrace )
import Foo (doFoo)

mainApp :: Trace IO Text -> IO ()
mainApp tr = doFoo (transformTextTrace tr)
```

To convert a `Trace m FooMsg` to anything else, use `contramap`.

```haskell
data AppMsg
    = FooMsg FooMsg
    | BarMsg BarMsg
    | TextMsg Text

mainApp :: Trace IO AppMsg -> IO ()
mainApp tr = doFoo (contramap (fmap FooMsg) tr)
```
