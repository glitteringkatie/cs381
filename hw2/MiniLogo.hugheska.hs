-- Katie  Hughes
-- Andrea Baldwin
-- Mikky  Cecil


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
type Prog  = [Cmd]

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



-- define line (x1, y1, x2, y2) {
--     pen up;
--     move (x1, y1);
--     pen down;
--     move (x2, y2);
--     pen up;
-- }

line :: Cmd
line = Define "line" ["x1","y1","x2","y2"] [
    Pen Up,
    Move (Ref "x1") (Ref "y1"),
    Pen Down,
    Move (Ref "x2") (Ref "y2"),
    Pen Up]

-- define nix (x, y, w, h) {
--     line(x, y, (x+w), (y+h));
--     line(x, (y+h), (x+w), y);
-- }

nix :: Cmd
nix = Define "nix" ["x","y","w","h"] [
    Call "line" [Ref "x",
               Ref "y",
               (Ref "x") `Plus` (Ref "w"),
               (Ref "y") `Plus` (Ref "h")],
    Call "line" [Ref "x",
               (Ref "y") `Plus` (Ref "h"),
               (Ref "x") `Plus` (Ref "w"),
               Ref "y"] ]

steps :: Int -> Prog
steps 0 = []
steps x = steps (x - 1) ++ [Call "line" [LitN x, LitN x, LitN (x - 1), LitN x],
          Call "line" [LitN (x - 1), LitN x, LitN (x - 1), LitN (x - 1)]]

macros :: Prog -> [Macro]
macros [] = []
macros ((Define m _ _):xs) = m:(macros xs)



tab = "    "
{-data Cmd  = Pen Mode-}
          {-| Move Expr Expr-}
          {-| Define Macro [Var] Prog-}
          {-| Call Macro [Expr]-}
     {-deriving (Eq, Show)-}
pretty :: Prog -> String
pretty [] = []
pretty ((Pen Up):xs)        = tab ++ "pen up;\n" ++ pretty xs
pretty ((Pen Down):xs)      = tab ++ "pen down;\n" ++ pretty xs
pretty ((Move ex1 ex2):xs)  = tab ++ "move (" ++ prettyExpr ex1 ++ "," ++ prettyExpr ex2 ++ ");\n" ++ pretty xs
pretty ((Define m vs p):xs) = "define " ++ m ++ "(" ++ varsToString vs ++ ") {\n" ++ pretty p ++ "}\n" ++ pretty xs
pretty ((Call m exs):xs)    = tab ++ "call " ++ m ++ "(" ++ arrayToString exs ++ ");\n" ++ pretty xs

{-data Expr = Ref Var-}
          {-| LitN Num-}
          {-| Plus Expr Expr-}
     {-deriving (Eq, Show)-}
prettyExpr :: Expr -> String
prettyExpr (Ref x)    = x
prettyExpr (LitN x)   = show x
prettyExpr (Plus x y) = prettyExpr x ++ "+" ++ prettyExpr y

arrayToString :: [Expr] -> String
arrayToString []     = ""
arrayToString [x]    = prettyExpr x
arrayToString (x:xs) = prettyExpr x ++ "," ++ arrayToString xs

varsToString :: [Var] -> String
varsToString []     = ""
varsToString [v]    = v
varsToString (v:xs) = v ++ "," ++ varsToString xs
