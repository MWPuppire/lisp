# Lisp

This is my attempt at writing a LISP interpreter, following [mal](https://github.com/kanaka/mal). I've deviated from the spec in a few places (intentionally; there are also many bugs causing more differences, of course). Some functions exist in my stdlib that aren't in Mal (`//` for integer division, `typeof` to get a string representation of the value's type, and `inspect` to get a string that, if `eval`ed, should return an equal value [except for atoms, functions, and macros, which compare by address and not structure]), and some functions behave differently.

## Known issues

Step 9 has failing tests with undefined variables in `symbol?`, which I assume means my evaluator is too eager in evaluating expressions.

`im` violates Stacked Borrows. I'd be surprised if I don't also, but I can never test that far under MIRI.

The code has memory leaks. Testing under MIRI reveals Arcs leaking in some of the tests, starting at step 4. It wouldn't surprise me if those are circular references, but I'm not sure what to do about those (other than switching to GC, which may actually be the best move).
