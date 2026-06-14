{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Lexer.Token(module Tiger.Lexer.Internal.Token) where

import Tiger.Lexer.Internal.Token(
    delex
  , SourceLoc(column, lineNumber, sourceFile, SourceLoc)
  , Token(loc, Token, typ)
  , TokenType(..)
  )
