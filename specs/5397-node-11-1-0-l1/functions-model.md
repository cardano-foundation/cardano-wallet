# Functions model — #5397 L1

No wallet function is designed by this ticket. The only expected signature
adaptation replaces function-style `runTracer` call sites with `traceWith` at
the existing emitted-value boundary, preserving the original tracer and value
arguments.
