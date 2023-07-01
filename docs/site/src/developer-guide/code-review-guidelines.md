# Code Review Guidelines

## Table of Content

- [Code Review Guidelines](#code-review-guidelines)
  - [Table of Content](#table-of-content)
  - [As a Reviewer or Author](#as-a-reviewer-or-author)
    - [DO: Assume competence.](#do-assume-competence)
    - [DO: Provide rationale or context](#do-provide-rationale-or-context)
    - [DO: Consider how comments may be interpreted.](#do-consider-how-comments-may-be-interpreted)
    - [DON’T: Criticize the person.](#dont-criticize-the-person)
    - [DON’T: Use harsh language.](#dont-use-harsh-language)
  - [As a Reviewer](#as-a-reviewer)
    - [DO: Provide specific and actionable feedback.](#do-provide-specific-and-actionable-feedback)
    - [DO: Clearly mark nitpicks and optional comments.](#do-clearly-mark-nitpicks-and-optional-comments)
  - [As an Author](#as-an-author)
    - [DO: Clarify code or reply to the reviewer’s comment.](#do-clarify-code-or-reply-to-the-reviewers-comment)
    - [DO: When disagreeing with feedback, explain the advantage of your approach.](#do-when-disagreeing-with-feedback-explain-the-advantage-of-your-approach)

## As a Reviewer or Author

### DO: Assume competence.

An author’s implementation or a reviewer’s recommendation may be due to the other party having different context than you. Start by asking questions to gain understanding.

### DO: Provide rationale or context

Such as a best practices document, a style guide, or a design document. This can help others understand your decision or provide mentorship.

### DO: Consider how comments may be interpreted.

Be mindful of the differing ways hyperbole, jokes, and emojis may be perceived.

e.g.:

Authors Don’t | Authors Do
--- | ---
I prefer short names so I’d rather not change this. Unless you make me? :) | Best practice suggests omitting obvious/generic terms. I’m not sure how to reconcile that advice with this request.

### DON’T: Criticize the person.

Instead, discuss the code. Even the perception that a comment is about a person (e.g., due to using “you” or “your”) distracts from the goal of improving the code.

e.g.:

Reviewers Don’t | Reviewers Do
--- | ---
Why are you using this approach? You’re adding unnecessary complexity. | This concurrency model appears to be adding complexity to the system without any visible performance benefit.

### DON’T: Use harsh language.

Code review comments with a negative tone are less likely to be useful. For example, prior research found very negative comments were considered useful by authors 57% of the time, while more-neutral comments were useful 79% of the time.

## As a Reviewer

### DO: Provide specific and actionable feedback.

If you don’t have specific advice, sometimes it’s helpful to ask for clarification on why the author made a decision.

e.g.:

Reviewers Don’t | Reviewers Do
--- | ---
I don’t understand this. | If this is an optimization, can you please add comments?


### DO: Clearly mark nitpicks and optional comments.

By using prefixes such as ‘Nit’ or ‘Optional’. This allows the author to better gauge the reviewer’s expectations.

## As an Author

### DO: Clarify code or reply to the reviewer’s comment.

In response to feedback, failing to do so can signal a lack of receptiveness to implementing improvements to the code.

e.g.

Authors Don’t | Authors Do
--- | ---
That makes sense in some cases butnot here. | I added a comment about why it’s implemented that way.

### DO: When disagreeing with feedback, explain the advantage of your approach.

In cases where you can’t reach consensus, bring the discussion on Slack with other peers from the team.

<hr/>

From an original source: https://testing.googleblog.com/2019/11/code-health-respectful-reviews-useful.html
