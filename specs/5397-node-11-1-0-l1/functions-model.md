# Functions model — #5397 L1

No wallet function is designed by this ticket. The tracing compatibility
adaptations preserve the existing values, types, exports, and control flow:
function-style `runTracer` becomes `traceWith`, and legacy `Tracer` function
constructors wrap `TA.emit` from `Control.Tracer.Arrow`.
