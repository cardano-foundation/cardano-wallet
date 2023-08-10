# 1. Use an effect system

Date: 2023-07-21

## Status

decided

## Context

We want a test scenario to be described using a modular eDSL as a composition of
other mini-eDSLs. This way it stays a high-level abstraction, decoupled from the 
details associated with interpreting it in terms of a real-world side effects.

## Decision

Use the [`effectful`](https://hackage.haskell.org/package/effectful-2.2.2.0) extensible effect library.

## Consequences

> As always, there's no free lunch. The Eff monad doesn't support effect handlers that require the ability to suspend or capture the rest of the computation and resume it later (potentially multiple times). This prevents effectful from providing (in particular):
> * A NonDet effect handler that executes multiple Alternative branches and collects their results.
> * A Coroutine effect.

>  It needs to be noted however that such NonDet effect handler in existing libraries is broken and none of the ones with support for higher order effects provide the Coroutine effect, so arguably it's not a big loss.
