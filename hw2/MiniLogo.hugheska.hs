-- num   ::= any natural number               -> This can be num or int
-- var   ::= any variable name                -> This can be a string
-- macro ::= any macro name
-- prog  ::= epsilon | cmd; prog              -> sequence of commands
-- mode  ::= `down` | `up`                    -> pen status
-- expr  ::= var                              -> variable reference
--         | num                              -> literal number
--         | expr + expr                      -> additional expression
-- cmd   ::= `pen` mode                       -> change pen mode
--         | `move` (expr, expr)              -> move pen to a new position
--         | `define` macro ( var* ) { prog } -> define a macro
--         | `call` macro ( expr* )           -> invoke a macro

import Prelude hiding (Num)

type Num   = Int
type Var   = String
type Macro = String
type Prog  = [Cmd] -- sequence of cmd

data Mode = Up | Down
     deriving (Eq, Show)

data Expr = Ref Var
          | LitN Num
          | Plus Expr Expr
     deriving (Eq, Show)

data Cmd  = Pen Mode
          | Move Expr Expr
          | Define Macro [Var] Prog
          | Call Macro [Expr]
     deriving (Eq, Show)

--line = define "line" (x,y,j,k){
--    pen up; move (x,y);
--    pen down; move (j,k);
--}

--nix = define "nix" (x,y,h,w){
--	line(x,y,x+w,y+h); 
--	line(x+w,y,x,y+h);
--}

