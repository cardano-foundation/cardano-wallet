The team follows a methodology known as [extreme programming][xp] which can be summarized as follows:

## Planning

- The project is divided into iterations of 1 week
- Releases are small and occur often (every 4 weeks at most)
- User stories are specified, understood, estimated and agreed upon upfront during a release planning
- An iteration planning starts each iteration:
    - bugs or failing tests to be fixed from previous iteration become tasks
    - one or more user stories are selected and broken down into tasks 
    - tasks are estimated and prioritized; tasks that are too big are broken down further
    - At the end of an iteration, there should be no tasks left 
- Project velocity is measured for each iteration
- Tasks, PR & Bugs all have a dedicated GitHub template:
    - [Task Template](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/ISSUE_TEMPLATE/task.md)
    - [Bug Template](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/ISSUE_TEMPLATE/bug.md)
    - [PR Template](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/PULL_REQUEST_TEMPLATE.md)
- Tasks and Bugs move across the following board:

```
|*************|  |*************|  |*************|  |*************|
|   Backlog   |  | In Progress |  |      QA     |  |    Closed   |
|-------------|  |-------------|  |-------------|  |-------------|
|             |  |             |  |             |  |             |
|     ...    ----->    ...    ----->    ...    ----->     ...    |
|_____________|  |_____________|  |_____________|  |_____________|
```


## Coding

- We work in pairs, each pair member reviewing the work of the other
- The code is collectively owned, everyone is knowledgeable about every part of the code
- Code is following an agreed [standard and style][styleguide]
- Code is integrated and tested daily to the main branch (`master`) through PR
- The main branch should be _releasable_ at any time and not contain broken features
- Any user-facing change (API change, Documentation, New Feature) should have be integrated with a corresponding entry in the CHANGELOG 
- No unplanned optimizations, features or unneeded abstractions are implemented
- We favor simple unbloated code and use refactoring techniques to add features 
- We favor test-driven and type-driven development 
    - High-level code API is written first with dummy or placeholder implementation
    - Tests are written using the API, ajusting when necessary and iterating until satisfaction
    - Tests fail (it's important to see test failing, to make sure they're actually testing something) 
    - The source implementation is completed 
    - Tests pass (giving confidence that we have useful tests with a correct implementation)


## QA 

- All code should be covered by tests (either unit, integration or manual) 
- Issues are closed by QA, once convinced by developers that the added code works and is covered
- When a bug is found, tests are created to illustrate the failure, prior to fixing it
- Tests are ran daily in a integration environment


## Communication

- We have daily written, asynchronous, stand-up on Slack on a separate channel
- A [blackboard page][blackboard] on our Wiki is dedicated to keep track of ideas and thoughts
- We have a weekly meeting every Friday to plan the next iteration
- We have release meetings between release to
    - Plan the next release (i.e. create or refine User Stories)
    - Retrospect on the previous release looking at past User Stories 
    - Discuss idea an thoughts gathered on the blackboard 
- Discussions happen on Slack, decisions are documented on GitHub as comments on issues
- Our GitHub wiki can be extended at any time with insights and details about the software
- Reports and metrics about the project are available to anyone
- A report is produced as a result of each iteration which contains:
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
[blackboard]: https://github.com/input-output-hk/cardano-wallet/wiki/Blackboard
[xp]: http://www.extremeprogramming.org