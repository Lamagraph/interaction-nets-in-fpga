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

    -- * Names
    -- $names

    -- * Type expressions
    -- $types

    -- * Constants
    -- $constants

    -- * Patterns
    -- $patterns

    -- * Expressions
    -- $expressions

    -- * Type definitions
    -- $typedefs

    -- * Declarations
    -- $decls

    -- * Missing things
    -- $missing
) where

{- $rules
Grammar rules are written in monospace font.
Italicized words represent @/nonterminals/@.
@Terminals@ are written in upright font.
@( )@ mean grouping, @{ }@ mean zero or more repetitions, @[ ]@ mean optional.
In case of clash between grammar and terminal meaning, terminal symbol will be typeset as @__bold monospace__@.
-}

{- $lexing_blanks
Spaces, tabs, @\\n@ and @\\r@ are ignored and are used to separate tokens.
-}

{- $lexing_comments

Single-line comments use @--@ syntax and consume ALL characters until line break.

Multiline comments must be enclosed in @(*@ and @*)@.
Nesting is allowed and must be handled.

Note that the following code won't be treated as a valid multiline comment!

@
-- (*
*)
@
-}

{- $lexing_idents
@
/ident/ ::= ( /letter/ | _ ) { /letter/ | 0...9 | _ | ' }

/capitalized-ident/ ::= ( A...Z ) { /letter/ | 0...9 | _ | ' }

/lowercase-ident/ ::= ( a...z | _ ) { /letter/ | 0...9 | _ | ' }

/letter/ ::= A...Z | a...z
@
-}

{- $lexing_int_lits
@
/integer-literal/ ::= \[-] ( 0...9 ) { 0...9 | _ }

/int32-literal/ ::= /integer-literal/ l

/uint32-literal/ ::= /integer-literal/ ul

/int64-literal/ ::= /integer-literal/ L

/uint64-literal/ ::= /integer-literal/ UL
@
-}

{- $lexing_char_lits
@
/char-literal/ ::= ' /regular-char/ '
               | ' /escape-sequence/ '

/escape-sequence/ ::= \\ ( __|__ | " | ' | n )
@

@/regular-char/@ must match every printable ASCII character (decimal range: 32-126).
-}

{- $lexing_string_lits
@
/string-literal/ ::= " { /string-character/ } "

/string-character/ ::= /regular-string-character/
                   | /escape-sequence/
@

@/regular-string-character/@ must match every printable ASCII character (decimal range: 32-126).
-}

{- $lexing_operators

@
/infix-symbol/ ::= ( = | \< | \> | @ | ^ | __|__ | & | + | - | * | / | $ | % ) { /operator-char/ }

/prefix-symbol/ ::= ! { /operator-char/ }
                | ( ? | ~ ) { /operator-char/ }+

/operator-char/ ::= ! | $ | % | & | * | + | - | . | / | : | \< | = | \> | ? | @ | ^ | __|__ | ~
@

Copypasted from https://askra.de/software/ocaml-doc/4.02/lex.html#sec71, probably too complicated.
-}

{- $lexing_keywords
Keyword table:

@
and asr begin else end false fun function
if in land let lor lsl lsr lxor match mod
of rec then true type when with

!= && ' ( ) * + , - -> : :: ; < = > [ ]
_ { | }
@

/I hope nothing is missing./
-}

{- $names
/No modules currently!/

@
/value-name/ ::= /lowercase-ident/
             | __(__ /operator-name/ __)__

/operator-name/ ::= /prefix-symbol/ | /infix-op/

/infix-op/ ::= /infix-symbol/
           | * | + | - | = | != | \< | \> | || | &&
           | mod | land | lor | lxor | lsl | lsr | asr

/constr-name/ ::= /capitalized-ident/

/typeconstr-name/ ::= /lowercase-ident/

@

/Revise or \/ || \/ lor/
-}

{- $types
@
/typexpr/ ::= ' /ident/
          | _
          | __(__ /typexpr/ __)__
          | /typexpr/ -> /typexpr/
          | /typexpr/ { * /typexpr/}+
          | /typeconstr-name/
          | /typexpr/ /typeconstr-name/
          | __(__ /typexpr/ {, /typexpr/} __)__ /typeconstr-name/
@
-}

{- $constants
@
/constant/ ::= /integer-literal/
           | /int32-literal/
           | /uint32-literal/
           | /int64-literal/
           | /uint64-literal/
           | /char-literal/
           | /string-literal/
           | /constr-name/
           | false
           | true
           | __()__
           | begin end
           | __[]__
@
-}

{- $patterns
@
/pattern/ ::= /value-name/
          | _
          | /constant/
          | __(__ /pattern/ __)__
          | __(__ /pattern/ : /typexpr/ __)__
          | /pattern/ __|__ /pattern/
          | /constr-name/ /pattern/
          | /pattern/ { , /pattern/ }
          | __[__ /pattern/ { ; /pattern/ } [;] __]__
          | /pattern/ :: /pattern/
@
-}

{- $expressions

@
/expr/ ::= /value-name/
       | /constant/
       | __(__ /expr/ __)__
       | begin /expr/ end
       | /expr/ {, /expr/ }
       | /constr-name/ /expr/
       | /expr/ :: /expr/
       | __[__ /expr/ { ; /expr/ } [;] ]
       | /expr/ { /argument/ }+
       | /prefix-symbol/ /expr/
       | - /expr/
       | /expr/ /infix-op/ /expr/
       | if /expr/ then /expr/ else /expr/
       | match /expr/ with /pattern-matching/
       | function /pattern-matching/
       | fun { /parameter/ }+ [ : /typexpr/ ] -> /expr/
       | let [rec] /let-binding/ { and /let-binding/ } in /expr/

/argument/ ::= /expr/

/pattern-matching/ ::= [ __|__ ] /pattern/ [ when /expr/ ] -> /expr/ { __|__ /pattern/ [ when /expr/ ] -> /expr/ }

/let-binding/ ::= /pattern/ = /expr/
              | /value-name/ { /parameter/ } [ : /typexpr/ ] = /expr/

/parameter/ ::= /pattern/
@
-}

{- $typedefs

@
/type-definition/ ::= type /typedef/ { and /typedef/ }

/typedef/ ::= [ /type-params/ ] /typeconstr-name/ /type-information/

/type-information/ ::= [ /type-equation/ ] [ /type-representation/ ]

/type-equation/ ::= = /typexpr/

/type-representation/ ::= = [ __|__ ] /constr-decl/ { __|__ /constr-decl/ }
                      | = __|__

/type-params/ ::= /type-param/
              | __(__ /type-param/ {, /type-param/ __)__

/type-param/ ::= ' /ident/

/constr-decl/ ::= ( /constr-name/ | __[]__ | __(__::__)__ ) [ of /constr-args/ ]

/constr-args/ ::= /typexpr/ { * /typexpr/ }
@
-}

{- $decls

@
/decl/ ::= /expr/ | /type-definition/

/prog/ ::= { /decl/ }
@
-}

{- $missing

For the sake of simplicity this language currently lacks these know to the authors features:

* Multiline strings
* Less useful escape sequences like @\\t@
* Records
* Float numbers
-}
