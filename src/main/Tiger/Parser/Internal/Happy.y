{module Tiger.Parser.Internal.Happy(happy) where

import Prelude hiding (runState, State)

import Control.Monad(mzero)
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)

import Data.List.NonEmpty((<|))

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

  '&'   { Token.Token  Token.And           _ }
  Array { Token.Token  Token.Array         _ }
  ':='  { Token.Token  Token.Assign        _ }
  Break { Token.Token  Token.Break         _ }
  ':'   { Token.Token  Token.Colon         _ }
  ','   { Token.Token  Token.Comma         _ }
  '/'   { Token.Token  Token.Divide        _ }
  Do    { Token.Token  Token.Do            _ }
  '.'   { Token.Token  Token.Dot           _ }
  Else  { Token.Token  Token.Else          _ }
  End   { Token.Token  Token.End           _ }
  '='   { Token.Token  Token.Equals        _ }
  For   { Token.Token  Token.For           _ }
  Func  { Token.Token  Token.Function      _ }
  '>='  { Token.Token  Token.GreaterEquals _ }
  '>'   { Token.Token  Token.GreaterThan   _ }
  If    { Token.Token  Token.If            _ }
  In    { Token.Token  Token.In            _ }
  '{'   { Token.Token  Token.LeftBrace     _ }
  '['   { Token.Token  Token.LeftBracket   _ }
  '('   { Token.Token  Token.LeftParen     _ }
  '<='  { Token.Token  Token.LessEquals    _ }
  '<'   { Token.Token  Token.LessThan      _ }
  Let   { Token.Token  Token.Let           _ }
  '-'   { Token.Token  Token.Minus         _ }
  '*'   { Token.Token  Token.Multiply      _ }
  Nil   { Token.Token  Token.Nil           _ }
  '<>'  { Token.Token  Token.NotEquals     _ }
  Of    { Token.Token  Token.Of            _ }
  '|'   { Token.Token  Token.Or            _ }
  '+'   { Token.Token  Token.Plus          _ }
  '}'   { Token.Token  Token.RightBrace    _ }
  ']'   { Token.Token  Token.RightBracket  _ }
  ')'   { Token.Token  Token.RightParen    _ }
  ';'   { Token.Token  Token.Semicolon     _ }
  Then  { Token.Token  Token.Then          _ }
  To    { Token.Token  Token.To            _ }
  Type  { Token.Token  Token.Type          _ }
  Var   { Token.Token  Token.Var           _ }
  While { Token.Token  Token.While         _ }
  INT   { Token.Token (Token.Int        _) _ }
  IDENT { Token.Token (Token.Identifier _) _ }
  STR   { Token.Token (Token.Stringy    _) _ }

%%

expr :: { AST.Expr }
expr
  : INT    { let Token.Token (Token.Int     value) _ = $1 in AST.IntExpr    (fromIntegral value) $1 }
  | STR    { let Token.Token (Token.Stringy value) _ = $1 in AST.StringExpr value                $1 }
  | Nil    { AST.NilExpr $1 }
  | lvalue { AST.LValueExpr $1 $1.lValToken }

  | '-' expr %prec NEG { AST.OpExpr (AST.IntExpr 0 $1) AST.MinusOp $2 $1 }

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

  | expr '&' expr { AST.IfExpr $1 $3                 (Just $ AST.IntExpr 0 $2) $2 }
  | expr '|' expr { AST.IfExpr $1 (AST.IntExpr 1 $2) (Just                 $3) $2 }

  | lvalue ':=' expr { AST.AssignExpr $1 $3 $2 }

  | IDENT '(' exprlist ')' { let name = ident $1 in AST.CallExpr name $3 $1 }
  | IDENT '('          ')' { let name = ident $1 in AST.CallExpr name [] $1 }

  | If expr Then expr           { AST.IfExpr $2 $4 Nothing   $1 }
  | If expr Then expr Else expr { AST.IfExpr $2 $4 (Just $6) $1 }

  | While expr Do expr { AST.WhileExpr $2 $4 $1 }

  | For IDENT ':=' expr To expr Do expr { let name = ident $2 in AST.ForExpr name True $4 $6 $8 $1 }

  | Break { AST.BreakExpr $1 }

  | '(' exprseq ')' { AST.SeqExpr $2 $1 }
  | '('         ')' { AST.SeqExpr [] $1 }

  | Let decls In exprseq End { AST.LetExpr (NE.reverse $2) (AST.SeqExpr $4 $3) $1 }
  | Let decls In         End { AST.LetExpr (NE.reverse $2) (AST.SeqExpr [] $3) $1 }

  | IDENT '{' fieldinits '}' { let name = ident $1 in AST.RecordExpr $3 name $1 }
  | IDENT '{'            '}' { let name = ident $1 in AST.RecordExpr [] name $1 }

  | IDENT '[' expr ']' Of expr { let name = ident $1 in AST.ArrayExpr name $3 $6 $1 }

  | '(' expr ')' { $2 }


lvalue :: { AST.LValue }
lvalue
  : IDENT               { let name = ident $1 in AST.Variable       name $1 }
  | lvalue '.' IDENT    { let name = ident $3 in AST.RecordField $1 name $3 }
  | lvalue '[' expr ']' { AST.ArrayIndex $1 $3 (AST.lValToken $1) }
  | IDENT  '[' expr ']' { let name = ident $1 in AST.ArrayIndex (AST.Variable name $1) $3 $1 }

exprlist :: { [AST.Expr] }
exprlist
  :              expr {       [$1] }
  | exprlist ',' expr { $1 <> [$3] }

exprseq :: { [(AST.Expr, Token.Token)] }
exprseq
  :             expr {       [($1, $1.token)] }
  | exprseq ';' expr { $1 <> [($3, $3.token)] }

fieldinits :: { [(AST.Symbol, AST.Expr, Token.Token)] }
fieldinits
  :                IDENT '=' expr { let name = ident $1 in       [(name, $3, $1)] }
  | fieldinits ',' IDENT '=' expr { let name = ident $3 in $1 <> [(name, $5, $3)] }

decls :: { NonEmpty AST.Decl }
decls
  : decl       { NE.singleton $1 }
  | decls decl { $2 <| $1 }

decl :: { AST.Decl }
decl
  : typedecl { AST.TypeDecl     $1 }
  | vardecl  { AST.VariableDecl $1 }
  | funcdecl { AST.FunctionDecl $1 }

typedecl :: { AST.TypeDeclEntry }
typedecl
  : Type IDENT '=' typ { let name = ident $2 in AST.TypeDeclEntry name $4 $2 }

typ :: { AST.Type }
typ
  : IDENT             { let name = ident $1 in AST.NamedType name $1 }
  | '{' fieldlist '}' { AST.RecordType $2 }
  | '{'           '}' { AST.RecordType [] }
  | Array Of IDENT    { let name = ident $3 in AST.ArrayType name $3 }

vardecl :: { AST.VarDecl }
vardecl
  : Var IDENT           ':=' expr { let name = ident $2 in AST.VarDecl name True Nothing $4 $2 }
  | Var IDENT ':' IDENT ':=' expr {
      let {
        varName  = ident $2;
        typeName = ident $4;
      } in AST.VarDecl varName True (Just (typeName, $4)) $6 $2
    }

funcdecl :: { AST.FuncDecl }
funcdecl
  : Func IDENT '(' fieldlist ')'           '=' expr { let name = ident $2 in AST.FuncDecl name $4 Nothing $7 $2 }
  | Func IDENT '('           ')'           '=' expr { let name = ident $2 in AST.FuncDecl name [] Nothing $6 $2 }
  | Func IDENT '(' fieldlist ')' ':' IDENT '=' expr {
      let {
        fName    = ident $2;
        typeName = ident $7;
      } in AST.FuncDecl fName $4 (Just (typeName, $7)) $9 $2
    }
  | Func IDENT '('           ')' ':' IDENT '=' expr {
      let {
        fName    = ident $2;
        typeName = ident $6;
      } in AST.FuncDecl fName [] (Just (typeName, $6)) $8 $2
    }

fieldlist :: { [AST.Field] }
fieldlist
  : IDENT ':' IDENT {
      let {
        fName    = ident $1;
        typeName = ident $3;
      } in [AST.Field fName True typeName $1 $3]
    }
  | fieldlist ',' IDENT ':' IDENT {
      let {
        fName    = ident $3;
        typeName = ident $5;
      } in $1 <> [AST.Field fName True typeName $3 $5]
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

ident (Token.Token (Token.Identifier name) _) = AST.Symbol name
ident                                       _ = error "Invalid match!  Boom!"
}
