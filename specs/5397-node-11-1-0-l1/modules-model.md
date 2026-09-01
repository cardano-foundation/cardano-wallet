# Modules model — #5397 L1

No component is introduced. Existing L1 package modules retain ownership.
`Cardano.BM.Extra` remains the `iohk-monitoring-extra` adapter to the tracing
API; its two call sites adapt to the existing `traceWith` surface supplied by
the pinned compatibility fork.
