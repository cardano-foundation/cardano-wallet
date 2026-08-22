# Data model

| ID | Data | Fields | Invariant |
| --- | --- | --- | --- |
| D1 | CHaP horizon | Git revision, index timestamp, content hash | I1 |
| D2 | Resolved dependency | package name, version, dependency edges | I2, I3 |

The Git revision must contain every package revision visible at the recorded
index timestamp. The resolved dependency record must name
`cardano-addresses` 4.0.8.
