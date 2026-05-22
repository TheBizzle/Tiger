{module Tiger.Parser.Internal.Happy(happy) where

import Prelude hiding (runState, State)

import Control.Monad(mzero)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)

import Tiger.Lexer.Token(
    SourceLoc(column, lineNumber, sourceFile, SourceLoc)
  , Token(loc, Token, typ)
  )

import Tiger.Parser.Internal.ParserError(ParserError(ParserError), ParserErrorType(Aborted, BadSyntax))

import qualified Data.List.NonEmpty        as NE
import qualified Tiger.Lexer.Token         as Token
import qualified Tiger.Parser.Internal.AST as AST
}

%name       happarse
%tokentype  { Token.Token }
%error      { abort } { report }
%monad      { ParseState } { (>>=) } { return }

%nonassoc do of
%nonassoc else -- Note the potential for dangling `else`
%left     '|'
%left     '&'
%nonassoc '=' '<>' '<' '<=' '>' '>='
%left     '+' '-'
%left     '*' '/'
%left     NEG  -- Specifically for unary minus; see `%prec` below

%token

  '&'   { Token.Token  Token.And           $$ }
  Array { Token.Token  Token.Array         $$ }
  ':='  { Token.Token  Token.Assign        $$ }
  Break { Token.Token  Token.Break         $$ }
  ':'   { Token.Token  Token.Colon         $$ }
  ','   { Token.Token  Token.Comma         $$ }
  '/'   { Token.Token  Token.Divide        $$ }
  Do    { Token.Token  Token.Do            $$ }
  '.'   { Token.Token  Token.Dot           $$ }
  Else  { Token.Token  Token.Else          $$ }
  End   { Token.Token  Token.End           $$ }
  '='   { Token.Token  Token.Equals        $$ }
  For   { Token.Token  Token.For           $$ }
  Func  { Token.Token  Token.Function      $$ }
  '>='  { Token.Token  Token.GreaterEquals $$ }
  '>'   { Token.Token  Token.GreaterThan   $$ }
  If    { Token.Token  Token.If            $$ }
  In    { Token.Token  Token.In            $$ }
  '{'   { Token.Token  Token.LeftBrace     $$ }
  '['   { Token.Token  Token.LeftBracket   $$ }
  '('   { Token.Token  Token.LeftParen     $$ }
  '<='  { Token.Token  Token.LessEquals    $$ }
  '<'   { Token.Token  Token.LessThan      $$ }
  Let   { Token.Token  Token.Let           $$ }
  '-'   { Token.Token  Token.Minus         $$ }
  '*'   { Token.Token  Token.Multiply      $$ }
  Nil   { Token.Token  Token.Nil           $$ }
  '<>'  { Token.Token  Token.NotEquals     $$ }
  Of    { Token.Token  Token.Of            $$ }
  '|'   { Token.Token  Token.Or            $$ }
  '+'   { Token.Token  Token.Plus          $$ }
  '}'   { Token.Token  Token.RightBrace    $$ }
  ']'   { Token.Token  Token.RightBracket  $$ }
  ')'   { Token.Token  Token.RightParen    $$ }
  ';'   { Token.Token  Token.Semicolon     $$ }
  Then  { Token.Token  Token.Then          $$ }
  To    { Token.Token  Token.To            $$ }
  Type  { Token.Token  Token.Type          $$ }
  Var   { Token.Token  Token.Var           $$ }
  While { Token.Token  Token.While         $$ }
  INT   { Token.Token (Token.Int        _)  _ }
  IDENT { Token.Token (Token.Identifier _)  _ }
  STR   { Token.Token (Token.Stringy    _)  _ }

%%

expr :: { AST.Expr }
expr
  : INT    { let Token.Token (Token.Int     value) srcLoc = $1 in AST.IntExpr    (fromIntegral value) srcLoc }
  | STR    { let Token.Token (Token.Stringy value) srcLoc = $1 in AST.StringExpr value                srcLoc }
  | Nil    { AST.NilExpr $1 }
  | lvalue { AST.LValueExpr $1 $1.lValSrcLoc }

  | '-' expr %prec NEG { AST.OpExpr (AST.IntExpr 0 $ SourceLoc "" 0 0) AST.MinusOp $2 $1 }

  | expr '+'  expr { AST.OpExpr $1 AST.PlusOp            $3 $2 }
  | expr '-'  expr { AST.OpExpr $1 AST.MinusOp           $3 $2 }
  | expr '*'  expr { AST.OpExpr $1 AST.TimesOp           $3 $2 }
  | expr '/'  expr { AST.OpExpr $1 AST.DivideOp          $3 $2 }
  | expr '='  expr { AST.OpExpr $1 AST.EqualsOp          $3 $2 }
  | expr '<>' expr { AST.OpExpr $1 AST.NotEqualsOp       $3 $2 }
  | expr '<'  expr { AST.OpExpr $1 AST.LessThanOp        $3 $2 }
  | expr '<=' expr { AST.OpExpr $1 AST.LessOrEqualsOp    $3 $2 }
  | expr '>'  expr { AST.OpExpr $1 AST.GreaterThanOp     $3 $2 }
  | expr '>=' expr { AST.OpExpr $1 AST.GreaterOrEqualsOp $3 $2 }

  | expr '&' expr { AST.IfExpr $1 $3                                 (Just $ AST.IntExpr 0 $ SourceLoc "" 0 0) $2 }
  | expr '|' expr { AST.IfExpr $1 (AST.IntExpr 1 $ SourceLoc "" 0 0) (Just                                 $3) $2 }

  | lvalue ':=' expr { AST.AssignExpr $1 $3 $2 }

  | IDENT '(' exprlist ')' { let Token.Token (Token.Identifier name) loc = $1 in AST.CallExpr name $3 loc }
  | IDENT '('          ')' { let Token.Token (Token.Identifier name) loc = $1 in AST.CallExpr name [] loc }

  | If expr Then expr           { AST.IfExpr $2 $4 Nothing   $1 }
  | If expr Then expr Else expr { AST.IfExpr $2 $4 (Just $6) $1 }

  | While expr Do expr { AST.WhileExpr $2 $4 $1 }

  | For IDENT ':=' expr To expr Do expr { let Token.Token (Token.Identifier name) _ = $2 in AST.ForExpr name True $4 $6 $8 $1 }

  | Break { AST.BreakExpr $1 }

  | '(' exprseq ')' { AST.SeqExpr $2 $1 }
  | '('         ')' { AST.SeqExpr [] $1 }

  | Let decls In exprseq End { AST.LetExpr $2 (AST.SeqExpr $4 $3) $1 }
  | Let decls In         End { AST.LetExpr $2 (AST.SeqExpr [] $3) $1 }

  | IDENT '{' fieldinits '}' { let Token.Token (Token.Identifier name) loc = $1 in AST.RecordExpr $3 name loc }
  | IDENT '{'            '}' { let Token.Token (Token.Identifier name) loc = $1 in AST.RecordExpr [] name loc }

  | IDENT '[' expr ']' Of expr { let Token.Token (Token.Identifier name) loc = $1 in AST.ArrayExpr name $3 $6 loc }

  | '(' expr ')' { $2 }


lvalue :: { AST.LValue }
lvalue
  : IDENT               { let Token.Token (Token.Identifier name) loc = $1 in AST.Variable name loc }
  | lvalue '.' IDENT    { let Token.Token (Token.Identifier name) loc = $3 in AST.RecordField $1 name loc }
  | lvalue '[' expr ']' { AST.ArrayIndex $1 $3 (AST.lValSrcLoc $1) }
  | IDENT  '[' expr ']' { let Token.Token (Token.Identifier name) loc = $1 in AST.ArrayIndex (AST.Variable name loc) $3 (Token.loc $1) }

exprlist :: { [AST.Expr] }
exprlist
  :              expr {       [$1] }
  | exprlist ',' expr { $1 <> [$3] }

exprseq :: { [(AST.Expr, Token.SourceLoc)] }
exprseq
  :             expr {       [($1, $1.srcLoc)] }
  | exprseq ';' expr { $1 <> [($3, $3.srcLoc)] }

fieldinits :: { [(AST.Symbol, AST.Expr, Token.SourceLoc)] }
fieldinits
  :                IDENT '=' expr { let Token.Token (Token.Identifier name) loc = $1 in       [(name, $3, loc)] }
  | fieldinits ',' IDENT '=' expr { let Token.Token (Token.Identifier name) loc = $3 in $1 <> [(name, $5, loc)] }

decls :: { [AST.Decl] }
decls
  : decl       {       [$1] }
  | decls decl { $1 <> [$2] }

decl :: { AST.Decl }
decl
  : typedecls { AST.TypeDecl $1 }
  | vardecl   { $1 }
  | funcdecls { AST.FunctionDecl $1 }

typedecls :: { [AST.TypeDeclEntry] }
typedecls
  : typedecl           {       [$1] }
  | typedecls typedecl { $1 <> [$2] }

typedecl :: { AST.TypeDeclEntry }
typedecl
  : Type IDENT '=' typ { let Token.Token (Token.Identifier name) _ = $2 in AST.TypeDeclEntry name $4 $1 }

typ :: { AST.TigerType }
typ
  : IDENT             { let Token.Token (Token.Identifier name) loc = $1 in AST.NamedType name loc }
  | '{' fieldlist '}' { AST.RecordType $2 }
  | '{'           '}' { AST.RecordType [] }
  | Array Of IDENT    { let Token.Token (Token.Identifier name) loc = $3 in AST.ArrayType name loc }

vardecl :: { AST.Decl }
vardecl
  : Var IDENT           ':=' expr { let Token.Token (Token.Identifier name) loc = $2 in AST.VarDecl name True Nothing $4 loc }
  | Var IDENT ':' IDENT ':=' expr {
      let {
        Token.Token (Token.Identifier  varName)  varLoc = $2;
        Token.Token (Token.Identifier typeName) typeLoc = $4;
      } in AST.VarDecl varName True (Just (typeName, typeLoc)) $6 varLoc }

funcdecls :: { [AST.FuncDecl] }
funcdecls
  : funcdecl           {       [$1] }
  | funcdecls funcdecl { $1 <> [$2] }

funcdecl :: { AST.FuncDecl }
funcdecl
  : Func IDENT '(' fieldlist ')'           '=' expr { let Token.Token (Token.Identifier name) loc = $2 in AST.FuncDecl name $4 Nothing $7 loc }
  | Func IDENT '('           ')'           '=' expr { let Token.Token (Token.Identifier name) loc = $2 in AST.FuncDecl name [] Nothing $6 loc }
  | Func IDENT '(' fieldlist ')' ':' IDENT '=' expr {
      let {
        Token.Token (Token.Identifier    fName)       _ = $2;
        Token.Token (Token.Identifier typeName) typeLoc = $7;
      } in AST.FuncDecl fName $4 (Just (typeName, typeLoc)) $9 $1
    }
  | Func IDENT '('           ')' ':' IDENT '=' expr {
      let {
        Token.Token (Token.Identifier    fName)       _ = $2;
        Token.Token (Token.Identifier typeName) typeLoc = $6;
      } in AST.FuncDecl fName [] (Just (typeName, typeLoc)) $8 $1
    }

fieldlist :: { [AST.Field] }
fieldlist
  : IDENT ':' IDENT {
      let {
        Token.Token (Token.Identifier    fName) fLoc = $1;
        Token.Token (Token.Identifier typeName)    _ = $3;
      } in [AST.Field fName True typeName fLoc]
    }
  | fieldlist ',' IDENT ':' IDENT {
      let {
        Token.Token (Token.Identifier    fName) fLoc = $3;
        Token.Token (Token.Identifier typeName)    _ = $5;
      } in $1 <> [AST.Field fName True typeName fLoc]
    }

{type ParseState a = MaybeT (State [ParserError]) a

happy :: [Token] -> Validation (NonEmpty ParserError) AST.Expr
happy tokens =
    case NE.nonEmpty errors of
      Just ne -> Failure $ NE.reverse ne
      Nothing ->
        case astM of
          Nothing  -> error "Unpossible"
          Just ast -> Success ast
  where
    (astM, errors) = runState (runMaybeT $ happarse tokens) []

abort :: [Token] -> ParseState a
abort tokens =
    do
      modify $ \errors -> (ParserError Aborted loc) : errors
      mzero
  where
    loc =
      case tokens of
        []    -> SourceLoc "" 0 0
        (h:_) -> h.loc

report :: [Token] -> ([Token] -> ParseState a) -> ParseState a
report tokens resume =
    do
      modify $ \errors -> (ParserError BadSyntax loc) : errors
      resume tokens
  where
    loc =
      case tokens of
        []    -> SourceLoc "" 0 0
        (h:_) -> h.loc
}
