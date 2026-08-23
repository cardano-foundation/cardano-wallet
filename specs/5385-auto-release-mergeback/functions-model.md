# Functions model

| ID | Function | Arguments | Result / effects |
| --- | --- | --- | --- |
| F1 | prepare merge-back | tag: text; repository: text | validated release identity or explicit failure; read-only |
| F2 | ensure merge-back PR | validated identity; PR metadata | existing PR identity or newly created PR identity; GitHub PR write only when absent |

Exact implementation names are not mandated; these rows define the changed
interfaces and effects.
