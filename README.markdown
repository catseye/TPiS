Total Procedures in Scheme (TPiS)
=================================

TPiS (Total Procedures in Scheme) is a static analyzer, written in Scheme,
which can check if given Scheme procedures are total (i.e. always terminate,
on any input) by checking that they are specified primitive-recursively.

What's more, it is written almost entirely in a primitive-recursive style,
so it can pass its own checks!

(There is one small Mulligan it must take, which is that it must guard
itself against its caller passing it a cyclic list, and it can't detect that
the code that implements this guard is total.  This would be a non-issue
in a purely functional dialect of Scheme where there were no such lists.)
