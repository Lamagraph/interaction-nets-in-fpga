{- |

= LamaML syntax

== Grammar rules

Grammar rules are written in monospace font.
Upright words represent @nonterminals@.
@\"Terminals"@ are written in quotes.
@( )@ mean grouping, @{ }@ mean zero or more repetitions, @[ ]@ mean optional.

== Lexical conventions

=== Blanks

Spaces, tabs, @\\n@ and @\\r@ are ignored and are used to separate tokens.

=== Comments

Comments must be enclosed in @(*@ and @*)@.
Nesting is allowed and must be handled.

=== Identifiers
@
ident ::= (letter | "_") {letter | "0"..."9" | "_" | "'"}

capitalized-ident ::= (\"A"...\"Z") {letter | "0"..."9" | "_" | "'"}

lowercase-ident ::= ("a"..."z" | "_") {letter | "0"..."9" | "_" | "'"}

letter ::= \"A"...\"Z" | "a"..."z"
@

=== Integer literals
@
integer-literal ::= \["-"] ("0"..."9") {"0"..."9" | "_"}
@

/Do we need hex\/octal\/binary numbers and\/or size suffixes?/

/Also no floats currently./

=== Character literals
@
char-literal ::= "'" regular-char "'"
               | "'" escape-sequence "'"

escape-sequence ::= "\" ("|" | """ | "'" | "n")
@

/Do we need more escape sequences?/
-}
module Syntax () where
