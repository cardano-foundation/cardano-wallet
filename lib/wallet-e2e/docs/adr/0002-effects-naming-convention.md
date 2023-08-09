# 2. Effects naming convention

Date: 2023-08-09

## Status

decided

## Context

We'd like to name effects in a consistent fashion such that they could be
distinguished from the corresponding domain types, e.g. `FxQuery` signinfies 
an effect whereas `Query` is a domain type.

## Decision

Use `Fx` prefix.  

For example:

- `FxTrace`
- `FxRandom`
- `FxQuery`

## Consequences

Effect names become 2 character longer but are readable, distinguishable
and consistent.
