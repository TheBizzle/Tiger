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

  <0> "&"           { simplyMake And           }
  <0> "array"       { simplyMake Array         }
  <0> ":="          { simplyMake Assign        }
  <0> "break"       { simplyMake Break         }
  <0> ":"           { simplyMake Colon         }
  <0> ","           { simplyMake Comma         }
  <0> "/"           { simplyMake Divide        }
  <0> "do"          { simplyMake Do            }
  <0> "."           { simplyMake Dot           }
  <0> "else"        { simplyMake Else          }
  <0> "end"         { simplyMake End           }
  <0> "="           { simplyMake Equals        }
  <0> "for"         { simplyMake For           }
  <0> "function"    { simplyMake Function      }
  <0> ">="          { simplyMake GreaterEquals }
  <0> ">"           { simplyMake GreaterThan   }
  <0> "if"          { simplyMake If            }
  <0> "in"          { simplyMake In            }
  <0> "{"           { simplyMake LeftBrace     }
  <0> "["           { simplyMake LeftBracket   }
  <0> "("           { simplyMake LeftParen     }
  <0> "<="          { simplyMake LessEquals    }
  <0> "<"           { simplyMake LessThan      }
  <0> "let"         { simplyMake Let           }
  <0> "-"           { simplyMake Minus         }
  <0> "*"           { simplyMake Multiply      }
  <0> "nil"         { simplyMake Nil           }
  <0> "<>"          { simplyMake NotEquals     }
  <0> "of"          { simplyMake Of            }
  <0> "|"           { simplyMake Or            }
  <0> "+"           { simplyMake Plus          }
  <0> "}"           { simplyMake RightBrace    }
  <0> "]"           { simplyMake RightBracket  }
  <0> ")"           { simplyMake RightParen    }
  <0> ";"           { simplyMake Semicolon     }
  <0> "then"        { simplyMake Then          }
  <0> "to"          { simplyMake To            }
  <0> "type"        { simplyMake Type          }
  <0> "var"         { simplyMake Var           }
  <0> "while"       { simplyMake While         }

  <0> $digit+                        { make $ Int . read . asString }
  <0> $alpha [$alpha $digit \_]*     { make Identifier }

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

{data AlexUserState =
  AlexUserState { stringBuffer :: [Text]
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
    initStringBuffer $ srcLocFrom line col
    alexSetStartCode str
    alexMonadScan

addToString :: (a, b, c, String) -> Int -> Alex Token
addToString (_, _, _, str) len =
  do
    appendStringBuffer $ Text.take len $ asText str
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
addControlEscape (_, _, _, str) len =
  do
    let c = toEnum $ fromEnum (str !! (len - 1)) - fromEnum '@'
    appendStringBuffer $ Text.singleton c
    alexMonadScan

addDecimalEscape :: AlexInput -> Int -> Alex Token
addDecimalEscape (_, _, _, str) len =
  do
    let n = read (asString $ Text.take (len - 1) $ Text.drop 1 $ asText str) :: Int
    appendStringBuffer $ Text.singleton $ toEnum n
    alexMonadScan

getString :: Alex (Text, SourceLoc)
getString = Alex $ \state -> Right (state, (buildStr state, buildSLoc state))
  where
    buildStr  state = Text.concat $ reverse $ (alex_ust state).stringBuffer
    buildSLoc state = Maybe.fromJust (alex_ust state).stringLoc

initStringBuffer :: SourceLoc -> Alex ()
initStringBuffer loc = Alex $ \state -> Right (update state, ())
  where
    update state =
      state {
        alex_ust = (alex_ust state) {
          stringBuffer = []
        , stringLoc    = Just loc
        }
      }

appendStringBuffer :: Text -> Alex ()
appendStringBuffer x = Alex $ \state -> Right (update state, ())
  where
    appendTo state = x:((alex_ust state).stringBuffer)
    update state   = state { alex_ust = (alex_ust state) { stringBuffer = appendTo state } }

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
getCommentDepth = Alex $ \state -> Right (state, commentDepth $ alex_ust state)

setCommentDepth :: Word -> Alex ()
setCommentDepth d = Alex $ \state -> Right (update state, ())
  where
    update state = state { alex_ust = (alex_ust state) { commentDepth = d } }

make :: (Text -> TokenType) -> AlexInput -> Int -> Alex Token
make transform (AlexPn _ line col, _, _, str) len =
  do
    let typ       = transform $ asText $ take len str
    let sourceLoc = srcLocFrom line col
    return $ Token typ sourceLoc

simplyMake :: TokenType -> AlexInput -> Int -> Alex Token
simplyMake typ input len = make (const typ) input len

srcLocFrom :: Int -> Int -> SourceLoc
srcLocFrom line column = SourceLoc "" (fromIntegral line) (fromIntegral column)

alexEOF :: Alex Token
alexEOF =
  do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    return $ Token EOF $ srcLocFrom line col

}
