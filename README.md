# Raskell
Raskell is a racket/scheme interpreter written in Haskell!

It was our final project for Programming Languages and Paradigms at the University of Kansas.

It has partial support for most of the most useful parts of scheme, including lists, strings, numbers, chars.
Full feature list:
 * `+-*/<>` for Integers
 * also >= and <=
 * equal? 
 * mod, quotient, remainder
 * Lists!
  * car
  * cdr
  * cons (although we don't support ```(cons '(something) 'anAtom)```, because left-leaning lists are NYI
 * Strings
  * string->list
  * list->string
 * symbol?
 * string?
 * number?
 * char?
 * ```list``` as a list constructor
 * null?
 * list?
 * define
 * set!
 * quote
 * cond
 * if
