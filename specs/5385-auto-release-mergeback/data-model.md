# Data model

| ID | Record | Fields | Invariants |
| --- | --- | --- | --- |
| D1 | Release identity | tag, peeled tag SHA, release-candidate branch, branch-head SHA | I1, I3 |
| D2 | Merge-back request | base, head, title, body, labels, assignee | I2, I4 |

An existing PR in any non-closed successful disposition satisfies D2 and makes
creation a no-op. A closed unmerged PR may be reported for maintainer action
rather than silently duplicated.
