module Tiger.Parser.Internal.Parser(parse) where

import Tiger.Lexer.Token(Token)

import Tiger.Parser.Internal.Happy(happy)
import Tiger.Parser.Internal.ParserError(ParserError)

import Tiger.Parser.Internal.AST qualified as AST


parse :: [Token] -> Validation (NonEmpty ParserError) AST.Expr
parse = happy
