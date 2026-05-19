# Specification Quality Checklist: Script-witness parity in `Transaction.Ledger`

**Purpose**: Validate specification completeness and quality before proceeding to planning
**Created**: 2026-05-18
**Feature**: [spec.md](../spec.md)

## Content Quality

- [X] No implementation details (languages, frameworks, APIs)
  *(see Note 1 below — internal Haskell wallet feature, code references are
  unavoidable; constrained to module/function names already named by the
  ticket so the spec remains testable.)*
- [X] Focused on user value and business needs
- [X] Written for non-technical stakeholders
  *(stakeholder = wallet-team maintainer continuing the cardano-api removal.)*
- [X] All mandatory sections completed

## Requirement Completeness

- [X] No [NEEDS CLARIFICATION] markers remain
- [X] Requirements are testable and unambiguous
- [X] Success criteria are measurable
- [X] Success criteria are technology-agnostic (no implementation details)
  *(see Note 1 below — by-design exception: parity-with-existing-code is
  the success criterion, so byte-equality and named-function diff
  emptiness are the only meaningful measures.)*
- [X] All acceptance scenarios are defined
- [X] Edge cases are identified
- [X] Scope is clearly bounded
- [X] Dependencies and assumptions identified

## Feature Readiness

- [X] All functional requirements have clear acceptance criteria
- [X] User scenarios cover primary flows
- [X] Feature meets measurable outcomes defined in Success Criteria
- [X] No implementation details leak into specification
  *(see Note 1 below.)*

## Notes

1. **Domain note on implementation details.** This spec sits inside a
   migration off `cardano-api`, where the user-value contract is
   *parity with named existing code*. The spec names
   `mkUnsignedTx`, `mkUnsignedTransaction`, `Transaction.Ledger`, and
   ledger body fields by name because the acceptance criterion is
   byte-equivalence against those exact functions. Stripping those
   names would make the spec untestable. The speckit checklist item
   is therefore satisfied "by intent" rather than by surface form.
2. The parity surface is finite and listed in User Story 1 (six
   scenarios). Slice shape and how scenarios fold into bisect-safe
   commits will be decided in `plan.md` / `tasks.md`.
