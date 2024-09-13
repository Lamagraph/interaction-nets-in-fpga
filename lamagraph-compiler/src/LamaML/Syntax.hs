{- |

LamaML syntax
-}
module LamaML.Syntax (
    -- * Grammar rules
    -- $rules

    -- * Lexical conventions

    -- ** Blanks
    -- $lexing_blanks

    -- ** Comments
    -- $lexing_comments

    -- ** Identifiers
    -- $lexing_idents

    -- ** Integer literals
    -- $lexing_int_lits

    -- ** Character literals
    -- $lexing_char_lits

    -- ** String literals
    -- $lexing_string_lits

    -- ** Operators
    -- $lexing_operators

    -- ** Keywords
    -- $lexing_keywords
) where

{- $rules
Grammar rules are written in monospace font.
Upright words represent @nonterminals@.
@\"Terminals"@ are written in quotes.
@( )@ mean grouping, @{ }@ mean zero or more repetitions, @[ ]@ mean optional.
-}

{- $lexing_blanks
Spaces, tabs, @\\n@ and @\\r@ are ignored and are used to separate tokens.
-}

{- $lexing_comments
Comments must be enclosed in @(*@ and @*)@.
Nesting is allowed and must be handled.
-}

{- $lexing_idents
@
ident ::= (letter | "_") {letter | "0"..."9" | "_" | "'"}

capitalized-ident ::= (\"A"...\"Z") {letter | "0"..."9" | "_" | "'"}

lowercase-ident ::= ("a"..."z" | "_") {letter | "0"..."9" | "_" | "'"}

letter ::= \"A"...\"Z" | "a"..."z"
@
-}

{- $lexing_int_lits
@
integer-literal ::= \["-"] ("0"..."9") {"0"..."9" | "_"}
@

/Do we need hex\/octal\/binary numbers and\/or size suffixes?/

/Also no floats currently./
-}

{- $lexing_char_lits
@
char-literal ::= "'" regular-char "'"
               | "'" escape-sequence "'"

escape-sequence ::= "\" ("|" | """ | "'" | "n")
@

/Do we need more escape sequences?/

/We must define regular-char? Any ASCII one?/
-}

{- $lexing_string_lits
@
string-literal ::= """ { string-character } """

string-character ::= regular-string-character
                   | escape-sequence
@

/We must define regular-string-character? Any ASCII one?/

/Do we need multiline strings?/
-}

{- $lexing_operators
revise later
-}

{- $lexing_keywords
revise later
-}
