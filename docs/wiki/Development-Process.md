<p align="center">
    <img alt="IOHK" src="iohk-signature.gif" />
</p>

The team's process is derived from [extreme programming][xp] and adapted for remote, distributed teams. It can be summarized as follows:

## Roadmap

- Our roadmap is a product backlog, owned by a _Product Owner_. 
- Each item is described in terms of:
    - A user story (U/S) following a Role-Feature-Reason template
    - Acceptance Criteria (A/C) written in [Gherkin](http://docs.behat.org/en/v2.5/guides/1.gherkin.html) 
    - Possible extra information or documents 
- Items in the backlog are sorted by priority. 
- When picked, U/S are estimated in terms of number of _sprints_. A story estimated to more than 3 sprints should be broken down into smaller stories. 

<details>
    <summary>example</summary>

> ### User Story 
> 
> **As** a stake pool operator  
> **I want** the pool ordering to be fair and not favor any particular pools especially during the bootstrapping era  
> **So that** every pool has the same chance to be selected by users in the early stages.
> 
> ### Acceptance Criteria 
> 
> **Given** that stake pools can be listed via https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listStakePools  
> **And** they are ordered by "apparent performance"  
> **When** I query stake pools during the first epoch (when little information about them is available)  
> **Then** pools are ordered arbitrarily  
> **And** the order is not necessarily the same between different wallets  
> **And** the order is consistent between successive calls within the same wallet.
</details>


## Iterations

- The project is divided into weekly iterations called _sprints_.
- Releases happen at the beginning of every sprints, Monday or Tuesday.
- Every 3 sprints, the team does 1 week of recovery time (See [Recovery Week](#recovery-week) below).
- User stories are assigned to and owned by a single member of the team (a.k.a the pilot). Pilots are seconded by a Co-pilot as follows:

    | Mission | Role | 
    | --- | --- | 
    | Clarify product requirements as needed with the product owner(s) | Pilot |
    | Break U/S into tasks (small, sizeable, chunks of work) | Pilot |
    | Estimate U/S in terms of # of sprints | Pilot |
    | Implement each task of a U/S | Pilot |
    | Challenge the task division and review it | Co-Pilot |
    | Primary reviewer of the development tasks | Co-Pilot | 
    | Call for assistance from peers when needed | Co-Pilot |
    | Challenge implementation decisions and technical choices | Co-Pilot |


- Tasks and Pull Requests have a dedicated GitHub template:
    - [Task Template](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/ISSUE_TEMPLATE/task.md)
    - [PR Template](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/PULL_REQUEST_TEMPLATE.md)
- Tasks move across the [following board](https://github.com/input-output-hk/cardano-wallet/projects/1) (see Task template for transitions)

```
|*************|  |*************|  |*************|  |*************|
|   Backlog   |  | In Progress |  |      QA     |  |    Closed   |
|-------------|  |-------------|  |-------------|  |-------------|
|             |  |             |  |             |  |             |
|     ...    ----->    ...    ----->    ...    ----->     ...    |
|_____________|  |_____________|  |_____________|  |_____________|
```

## Recovery Week

- Sprinters can't run all the time. During sprints, we often accumulate technical debts (e.g. `TODO` or `FIXME`).
- During recovery weeks, the team has a dedicated moment to tackle some of the technical debts. This includes:
    - Reviewing and extending code documentation
    - Refactoring some potentially entangled parts of the code
    - Re-organizing modules and folder achitecture
    - Fix small `TODOs` or `FIXMEs`, or, turn them into U/S 
    - Identify areas of the source code which needs improvement
- Recovery weeks happen instead of a sprint, and start with a retrospective meeting about the last 3 sprints.


## Coding

- The code is collectively owned, everyone is knowledgeable about every part of the code
- The code is peer-reviewed
- Code is following an agreed [standard and style][styleguide]
- Code is integrated and tested daily to the main branch (`master`) through PR
- The main branch should be _releasable_ at any time and not contain broken features
- No unplanned optimizations, features or unneeded abstractions are implemented
- We favor simple unbloated code and use refactoring techniques to add features 
- We test chunks of codes as we submit and integrate them, maintaining a high code coverage at all time


## QA 

- All code should be covered by tests (either unit, integration or manual). 
- We favor automated tests over manual testing.
- Issues are closed by QA, once convinced by developers that the added code works and is covered
    - Developers are expected to point relevant automated or manual test procedures to QA
    - Developers may also point to documentation or, code details that ensure reliability of the code
- When a bug is found, regression tests are created to illustrate the failure, prior to fixing it
- Tests are ran daily in a integration environment.
- Critical parts of the code have benchmarks to identify potential bottlenecks.  
- Code and more importantly public interfaces are well-documented and digestible.

## Bugs

- When a potential bug is found, a [Bug ticket](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/ISSUE_TEMPLATE/bug.md) is created with a label `BUG?` 
- Corresponding sections of the ticket are filled-in (context, reproduction path, expected behavior...)
- The bug is added to the [following board](https://github.com/input-output-hk/cardano-wallet/projects/2) in "Needs Triage"
- The ticket is discussed on Slack with the team to confirm that it's indeed a bug.
- Once confirmed, the label `BUG?` is changed to `BUG:CONFIRMED` and the bug is given a priority (either low or high).
- If dispelled, the bug ticket is closed without further ado.  
- When resolved, the bugs moved to the "QA" section of the bugs board.

## Communication

- We have daily written, asynchronous, stand-up on Slack on a separate channel
- Each Wednesday, an iteration meeting is done:
    - 1h max
    - To do a retrospective on past U/S and estimations. 
    - To assign new U/S to team members
    - To discuss important matters or change in the process 
- Every 3 sprints, the Wednesday meeting becomes a monthly retrospective where the team can discuss what went well, what didn't and take actions to improve things (see also https://www.retrospected.com/)
- Discussions happen on Slack in clear threads, decisions are documented on GitHub as comments on issues
- Our GitHub wiki can be extended at any time with insights and details about the software
- Reports and metrics about the project are available to anyone
- Every week, we produce a technical & non-technical report (see [weekly-reports](https://github.com/input-output-hk/cardano-wallet/tree/weekly-reports)) containing:
    - A brief non-technical summary of the week overview
    - A list of the completed user stories and their business value
    - A list of known issues or debt accumulated during the iteration

## Resources

#### Books

- [The Pragmatic Programmer](https://www.amazon.com/Pragmatic-Programmer-Journeyman-Master/dp/020161622X/) 
- [Refactoring: Improving the Design of Existing Code](https://www.amazon.com/Refactoring-Improving-Existing-Addison-Wesley-Signature/dp/0134757599/)
- [Clean Code](https://www.amazon.com/Clean-Code-Handbook-Software-Craftsmanship/dp/0132350882/)
- [Release It!](https://www.amazon.com/Release-Design-Deploy-Production-Ready-Software/dp/1680502395/)
- [Thinking With Types](https://leanpub.com/thinking-with-types/)
- [Git](https://www.amazon.com/Version-Control-Git-collaborative-development/dp/1449316387/)

#### Guides

- http://www.extremeprogramming.org
- https://try.github.io/
- https://12factor.net/


[styleguide]: https://github.com/input-output-hk/cardano-wallet/wiki/Coding-Standards
[xp]: http://www.extremeprogramming.org