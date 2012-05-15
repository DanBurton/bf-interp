bf-interp
=========

An interpreter for brainfuck.

I'm not quite sure what's wrong, but it doesn't work as expected.
I suspect the problem lies in the handling of [ and ]
though I'm not sure what the problem is.

I first tried implementing it with Cont,
then I switched to using MonadFix with MaybeT and State.
Both attempts produce incorrect output.
