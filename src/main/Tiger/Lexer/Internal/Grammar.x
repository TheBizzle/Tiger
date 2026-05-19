{{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Tiger.Lexer.Internal.Grammar where

import Data.List((!!), (++), drop, reverse, take)

import Tiger.Lexer.Internal.Token(
    SourceLoc(SourceLoc)
  , Token(Token)
  , TokenType(..)
  )

import qualified Data.Maybe as Maybe
import qualified Data.Text  as Text
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  <0> $white+     ;

  <0>       "/*"     { startComment }
  <comment> "/*"     { startComment }
  <comment> "*/"     { endComment   }
  <comment> .        ;
  <comment> \n       ;

  "&"                        { make $ const And           }
  "array"                    { make $ const Array         }
  ":="                       { make $ const Assign        }
  "break"                    { make $ const Break         }
  ":"                        { make $ const Colon         }
  ","                        { make $ const Comma         }
  "/"                        { make $ const Divide        }
  "do"                       { make $ const Do            }
  "."                        { make $ const Dot           }
  "else"                     { make $ const Else          }
  "end"                      { make $ const End           }
  "="                        { make $ const Equals        }
  "for"                      { make $ const For           }
  "function"                 { make $ const Function      }
  ">="                       { make $ const GreaterEquals }
  ">"                        { make $ const GreaterThan   }
  "if"                       { make $ const If            }
  "in"                       { make $ const In            }
  "{"                        { make $ const LeftBrace     }
  "["                        { make $ const LeftBracket   }
  "("                        { make $ const LeftParen     }
  "<="                       { make $ const LessEquals    }
  "<"                        { make $ const LessThan      }
  "let"                      { make $ const Let           }
  "-"                        { make $ const Minus         }
  "*"                        { make $ const Multiply      }
  "nil"                      { make $ const Nil           }
  "<>"                       { make $ const NotEquals     }
  "of"                       { make $ const Of            }
  "|"                        { make $ const Or            }
  "+"                        { make $ const Plus          }
  "}"                        { make $ const RightBrace    }
  "]"                        { make $ const RightBracket  }
  ")"                        { make $ const RightParen    }
  ";"                        { make $ const Semicolon     }
  "then"                     { make $ const Then          }
  "to"                       { make $ const To            }
  "type"                     { make $ const Type          }
  "var"                      { make $ const Var           }
  "while"                    { make $ const While         }

  $digit+                    { make $ Int . read . asString }
  $alpha [$alpha $digit \_]* { make Identifier }

  <0>   \"           { startString }
  <str> [^\"\\]+     { addToString }
  <str> \"           { endString   }
  <str> \\n          { addEscape '\n' }
  <str> \\t          { addEscape '\t' }
  <str> \\\\         { addEscape '\\' }
  <str> \\\"         { addEscape '"'  }
  <str> \\\^[@-_]    { addControlEscape }
  <str> \\[0-9]{3}   { addDecimalEscape }
  <str> \\$white+\\  ;

{data AlexUserState = AlexUserState
  { stringBuffer :: [Text]
  , stringLoc    :: Maybe SourceLoc
  , commentDepth :: Word
  }

alexInitUserState :: AlexUserState
alexInitUserState =
  AlexUserState { stringBuffer = []
                , stringLoc    = Nothing
                , commentDepth = 0
                }

alexScanTokens :: String -> Either String [Token]
alexScanTokens input =
    runAlex input loop
  where
    loop =
      do
        token <- alexMonadScan
        case token of
          Token EOF _ -> return []
          t           -> map (t:) loop

startString :: AlexInput -> b -> Alex Token
startString (AlexPn _ line col, _, _, _) _ =
  do
    let loc = srcLocFrom line col
    initStringBuffer loc
    alexSetStartCode str
    alexMonadScan

addToString :: (a, b, c, String) -> Int -> Alex Token
addToString (_, _, _, s) len =
  do
    appendStringBuffer $ Text.take len $ asText s
    alexMonadScan

endString :: a -> b -> Alex Token
endString _ _ =
  do
    alexSetStartCode 0
    (buffer, srcLoc) <- getString
    let str           = Stringy buffer
    return $ Token str srcLoc

addEscape :: Char -> AlexInput -> Int -> Alex Token
addEscape c _ _ =
  do
    appendStringBuffer $ Text.singleton c
    alexMonadScan

addControlEscape :: AlexInput -> Int -> Alex Token
addControlEscape (_, _, _, s) len =
  do
    let c = toEnum $ fromEnum (s !! (len - 1)) - fromEnum '@'
    appendStringBuffer $ Text.singleton c
    alexMonadScan

addDecimalEscape :: AlexInput -> Int -> Alex Token
addDecimalEscape (_, _, _, s) len =
  do
    let t = asText s
    let n = read (asString $ Text.take (len - 1) $ Text.drop 1 t) :: Int
    appendStringBuffer $ Text.singleton $ toEnum n
    alexMonadScan

getString :: Alex (Text, SourceLoc)
getString =
  Alex $ \s ->
    Right (
      s
    , (Text.concat $ reverse $ (alex_ust s).stringBuffer, Maybe.fromJust (alex_ust s).stringLoc)
    )

initStringBuffer :: SourceLoc -> Alex ()
initStringBuffer loc =
  Alex $ \s ->
    Right (s {
      alex_ust = (alex_ust s) {
        stringBuffer = []
      , stringLoc    = Just loc
      }
    }, ())

appendStringBuffer :: Text -> Alex ()
appendStringBuffer x =
  Alex $ \s -> Right (s { alex_ust = (alex_ust s) { stringBuffer = x:((alex_ust s).stringBuffer) } }, ())

startComment :: AlexInput -> Int -> Alex Token
startComment _ _ =
  do
    n <- getCommentDepth
    setCommentDepth $ n + 1
    alexSetStartCode comment
    alexMonadScan

endComment :: AlexInput -> Int -> Alex Token
endComment _ _ =
  do
    n <- getCommentDepth
    let m = n - 1
    setCommentDepth m
    if m == 0 then
      alexSetStartCode 0
    else
      return ()
    alexMonadScan

getCommentDepth :: Alex Word
getCommentDepth = Alex $ \s -> Right (s, commentDepth $ alex_ust s)

setCommentDepth :: Word -> Alex ()
setCommentDepth d = Alex $ \s -> Right (s { alex_ust = (alex_ust s) { commentDepth = d } }, ())

make :: (Text -> TokenType) -> AlexInput -> Int -> Alex Token
make transform (AlexPn _ line col, _, _, s) len =
  do
    let sourceLoc = srcLocFrom line col
    let typ       = transform $ asText $ take len s
    return $ Token typ sourceLoc

srcLocFrom :: Int -> Int -> SourceLoc
srcLocFrom line column = SourceLoc "" (fromIntegral line) (fromIntegral column)

alexEOF :: Alex Token
alexEOF =
  do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    return $ Token EOF (srcLocFrom line col)

}
