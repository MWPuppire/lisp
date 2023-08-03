# Lisp

This is my attempt at writing a LISP interpreter, following [mal](https://github.com/kanaka/mal). I've deviated from the spec in a few places; some functions exist in my stdlib that aren't in Mal, and some functions behave differently.

## Usage

The program can work as a REPL or run files; if passed arguments on the command-line, the first is treated as a file (unless it is `--`) and the rest are arguments, otherwise the REPL begins.

## Changes from mal

Additional functions in the standard library:

* `//` is the integer division function, dividing and truncating the result.
* `typeof` returns a string representation of the value's type (e.g. `"string"` or `"list"`).
* `inspect` returns a string that, if `eval`ed, should return a structurally equal value (which may not compare equal, since atoms, functions, and macros compare by address).
* `dump-env` returns a string that, if `eval`ed, would create an equivalent environment to the current one by redefining every variable to its current value.
* `pairs` takes a hash-map and returns a list of the map's key-value pairs (as lists).
* `join` takes a list of strings and a string separator and returns each string in the list joined by the separator.
* `and` takes any number of arguments, coerces each to a boolean, and returns the logical AND of the arguments. This is not short-circuited (i.e. every argument is always evaluated).
* `or` takes any number of arguments, coerces each to a boolean, and returns the logical OR of the arguments. This is not short-circuited (i.e. every argument is always evaluated).

Other differences:

* The REPL uses `inspect` instead of directly writing strings.
* `+`, `-`, and `*` accept multiple arguments (mal specifies only binary definitions for these).

## Known issues

`im` violates Stacked Borrows. It wouldn't surprise me if the LISP interpreter also does, but Miri never gets far enough testing before crashing from `im` (without `-Zmiri-disable-stacked-borrows`).

The code has memory leaks. Testing under Miri reveals Arcs leaking in some of the tests, starting at step 4. It wouldn't surprise me if those are circular references, but I'm not sure what to do about those (other than switching to GC, which may actually be the best move).

## License

The code is released under the MIT license.
