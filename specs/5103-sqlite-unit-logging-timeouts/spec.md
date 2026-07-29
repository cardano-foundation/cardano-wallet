# Specification — #5103 SQLite unit-test diagnostics

## User story

As a CI maintainer, I need a hanging SQLite unit-test job to identify the
exact example and its last known database or reference-count state, so that
the failure can be diagnosed from the first Buildkite log.

## Functional requirements

- FR-001: Every unit-test example whose fully-qualified Hspec path contains
  `Sqlite` emits a line when it starts and a line when it finishes.
- FR-002: Both progress lines include the fully-qualified example path and
  are flushed while the process is running.
- FR-003: SQLite examples that can block on a reference, connection, backup,
  or query use a 60-second timeout.
- FR-004: A timeout failure names the example and includes its latest
  diagnostic state.
- FR-005: Reference-count diagnostics identify the resource id, observed
  reference count, and wait/lock stage.
- FR-006: migration diagnostics identify the database file, current
  operation, and whether a connection or lock is expected to be open.
- FR-007: pure `PersistField` SQLite type tests are identified by progress
  logs but do not invent a database/lock state that they do not have.

## Success criteria

- SC-001: A focused SQLite run visibly prints paired `START` and `FINISH`
  lines for the Delete, Migration.New, and Types specs.
- SC-002: A seeded timeout test fails with the example name and injected
  diagnostic state in its error text.
- SC-003: A seeded formatter test observes both progress lines before and
  after an example.
- SC-004: The existing focused SQLite suite and the full repository gate
  pass after the change.

## Non-goals

- Changing production SQLite locking, retry, migration, or connection code.
- Adding retries that hide a hang.
- Logging SQL parameters or other potentially sensitive values.
