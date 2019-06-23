BUILDING SPEC 06 (x86)

Follow instructions : https://www.spec.org/cpu2006/Docs/runspec-avoidance.html

Errors(gcc 5.4.0) & Fixes:

1. perlbench
compile error: invalid use of '__builtin_va_arg_pack ()'
fix: pass '-std=gnu89' as a flag to gcc


