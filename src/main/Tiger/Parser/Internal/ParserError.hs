{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Parser.Internal.ParserError(
    ParserError(ParserError, peLoc, peType)
  , ParserErrorType(Aborted, BadSyntax)
  ) where

import Tiger.Lexer.Token(SourceLoc)


data ParserError =
  ParserError { peType :: ParserErrorType
              , peLoc  :: SourceLoc
              }
  deriving (Show)

data ParserErrorType
  = Aborted
  | BadSyntax
  deriving (Show)
