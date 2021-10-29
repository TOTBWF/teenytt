{
module TeenyTT.Frontend.Parser.Lexer where

import TeenyTT.Frontend.Parser.Token

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z09]

tokens :-

-- Whitespace insensitive
$white+                       ;

-- Comments
"#".*                         ;

-- Syntax

(λ|\\)                        { \_ -> Lambda }
:                             { \_ -> Colon }
(→|\->)                       { \_ -> Arrow }
(∀|\forall)                   { \_ -> ForAll }
\(                            { \_ -> LParen }
\)                            { \_ -> RParen }
$alpha [$alpha $digit \_ \-]* { \s -> Identifier s }


{
lexer :: String -> [Token]
lexer = alexScanTokens
}