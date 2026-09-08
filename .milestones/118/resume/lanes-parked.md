# Lane park records — 2026-09-07

Three lanes were told to park with this order of operations, the first step
being the one that protects work:

1. **commit any uncommitted edits first**, with an honest `wip(NNNN): …`
   subject — an uncommitted tree is the one state a pause cannot protect, and
   this stack was already bitten once today by a rebase over a live tree;
2. write `PAUSED` with the exact head SHA, branch, PR, `mergeable`, last
   measured check counts, census and `MAX` from the gate's own output, open
   findings by name, and one next action;
3. stop. Panes stay alive. **Retire nothing** — a paused seat is not a finished
   one.

| lane | pane | window | PR |
|---|---|---|---|
| `t5422-delete-era-split` | `%673` | `cardano-wallet-ms6-t5422-delete-era-split` | #5428 |
| `t5429-converter-mirrors` | `%683` | `cardano-wallet-ms6-e5209-t5429-converter-mirrors` | #5430 |
| `t-native-scripts` | `%687` | `cardano-wallet-ms6-native-scripts` | #5432 |

Commit owners and inspectors park through their ticket owner, never directly
from this desk.

They were also told, in terms: **do not rush a fix to beat the pause.** A
hurried commit tonight is how tomorrow starts with a bad tree.
