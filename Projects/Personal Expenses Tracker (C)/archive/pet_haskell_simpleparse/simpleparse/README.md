simpleparse
===========

A dead simple parser combinator written in Haskell. Just use by
dropping Parser.hs (and LICENSE) into your project.

### Changelog 20140521

* [3df3ab3] Add the `updatePos` function as part of the `Parser` type.
* [f921949] Introduce `PosUpd a p` as an alias for positioning updates
  function.
* [eb824f8] Rename `Predicate a` to `Pred a`. Only internal alias.
* [79d50ee] Minor updates to comment strings.

### Changelog 20140520

* Minor LICENSE editing cleanup and fixes.
* Cleanup imports from the stylish-haskell mess.
* [543bf42] Remove `ErrorMessage` and `Description` aliases. Now using
  `String` instead.
* [11bd3bd] Parser is now a newtype rather than a data type.
* [28f7c75] Fix a bug in sat1 where EOF would have to explicitly be
  detected by the given parser instead of by sat1 itself.
* [1b095cb] Add failList where the parser's error type is now a list
  of `String` (errors) instead of a single string. This helps offload
  the burden of formatting error messages to a more toplevel function.
* [25b0055] Use failList in pSat1 and failEnd.
* [2a535a9] Add pos for saving parser position.
* [e051377] Add `failListPos` which is `failList` with a `pos` argument.
