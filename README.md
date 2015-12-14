# Raskell
Raskell is a racket/scheme interpreter written in Haskell!

It was our final project for Programming Languages and Paradigms at the University of Kansas.

To see a quick demonstration, pull this repo, compile with `make`, then run one of the following:
```
./rli example_scheme/disemvowel.rsk
(disemvowel "Some Words Here")
```
or
```
./rli example_scheme/hw3.rsk
(timesdepth* 1 '(1 2 3 4 (1 2 3 4 (1 2 3 4))))
```
(timesdepth was a previous homework in our class, so we chose it as the test for completeness.

It has partial support for most of the most useful parts of scheme, including lists, strings, numbers, chars.
Full feature list:
 * `+-*/<>` for Integers
 * also `>=` and `<=`
 * `equal?`
 * `mod`, `quotient`, `remainder`
 * Lists!
  * `car`
  * `cdr`
  * `cons` (although we don't support `(cons '(something) 'anAtom)`, because left-leaning lists are NYI
 * Strings
  * `string->list`
  * `list->string`
 * `symbol?`
 * `string?`
 * `number?`
 * `char?`
 * `list` as a list constructor
 * `null?`
 * `list?`
 * `define`
 * `set!`
 * `quote`
 * `cond`
 * `if`

Credit where credit is due: We had a lot of help and inspiration from the following site: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
Because of the nature of this project as a final assignment for a class, much care was taken to indicate when we were working without help from the website. It ended up requiring a lot of modifications anyway because we skipped some steps (like error handling and building a "proper" REPL)
