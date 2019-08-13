<p align="center">
    <img alt="IOHK" src="iohk-signature.gif" />
</p>

The team follows a methodology known as [extreme programming][xp] which can be summarized as follows:

## Roadmap

- Our Roadmap is specified [in our wiki](https://github.com/input-output-hk/cardano-wallet/wiki/Roadmap)
- Items on the Roadmap are prioritized
- The Roadmap reflects user-stories (and not technical details)
- The Roadmap is reviewed and discussed with the product team

## Planning

- The project is divided into iterations of 2 weeks
- Releases are small and occur often (every 4 weeks at most)
- Technical discussions and proposals happen in a dedicated repository:
    - [cardano-wallet-adr](https://github.com/input-output-hk/cardano-wallet-adr)
- User stories are assigned to and owned by a member of the team. User story's owners are expected to:
    - Clarify product requirements as needed with the product referent
    - Break the U/S into tasks (small, sizeable, chunks of work)
    - Make sure that all tasks are estimated and clear
    - Make sure that a story can fit in a sprint, or, reduce its scope as needed
    - Make decisions regarding implementation and technical solutions for that U/S
- An iteration planning starts each iteration:
    - U/S are discussed and reviewed by the whole team
    - Proposed technical solutions in relation with U/S are reviewed 
    - New technical problems arise and are discussed in [cardano-wallet-adr](https://github.com/input-output-hk/cardano-wallet-adr) 
- Project velocity is measured for each iteration and help estimating how big can be a single iteration
- Tasks and PR have a dedicated GitHub template:
    - [Task Template](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/ISSUE_TEMPLATE/task.md)
    - [PR Template](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/PULL_REQUEST_TEMPLATE.md)
- Tasks move across the following board (see Task template for transitions)

```
|*************|  |*************|  |*************|  |*************|
|   Backlog   |  | In Progress |  |      QA     |  |    Closed   |
|-------------|  |-------------|  |-------------|  |-------------|
|             |  |             |  |             |  |             |
|     ...    ----->    ...    ----->    ...    ----->     ...    |
|_____________|  |_____________|  |_____________|  |_____________|
```

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

- All code should be covered by tests (either unit, integration or manual) 
- Issues are closed by QA, once convinced by developers that the added code works and is covered
    - Developers are expected to point relevant automated or manual test procedures to QA
    - Developers may also point to documentation or, code details that ensure reliability of the code
- When a bug is found, tests are created to illustrate the failure, prior to fixing it
- Tests are ran daily in a integration environment

## Bugs

- When a potential bug is found, a [Bug ticket](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/ISSUE_TEMPLATE/bug.md) is created with a label `UNCONFIRMED`.
- Corresponding sections of the ticket are filled-in (context, reproduction path, expected behavior...)
- The ticket is discussed on Slack with the team to confirm that it's indeed a bug.
- Once confirmed, the label `UNCONFIRMED` is changed to `CONFIRMED` and the bug is given a priority.
- If dispelled, the bug ticket is closed without further ado.  
- Bugs then move across the same board as tasks, described previously.

## Communication

- We have daily written, asynchronous, stand-up on Slack on a separate channel
- A [blackboard page][blackboard] on our Wiki is dedicated to keep track of ideas and thoughts
- We have a bi-monthly meeting every other Wednesday to plan the next iteration
- Discussions happen on Slack, decisions are documented on GitHub as comments on issues
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
[blackboard]: https://github.com/input-output-hk/cardano-wallet/wiki/Blackboard
[xp]: http://www.extremeprogramming.org