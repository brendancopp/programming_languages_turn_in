type Program = [Statement] -- a program is a list of statements

data Statement
  = Assignment Identifier Expression
  | If Condition Statement Statement
  | While Condition Statement
  | Msg String
  | Msg String
  | Print Expression
  | Println Expression
  | Read Identifier
  | Skip
  | Block Program

type Identifer = String

-- This is not correct
data ConditionBool
  = BoolConst Bool
  | Not Condition
  | BoolBinOp Condition Condition

data ConditionExpression
  | Test Comparison Expression Expression
  | Test Comparison Expression Condition

data BoolBinOp
  = And
  | Or

data Comparison
  = Greater
  | GreaterEq
  | Less
  | LessEqual
  | Equal
  | NEqual

-- This is not correct
data Expression
  = Var Identifier					-- a = 100
  | IntConst Integer				-- 10
  | Unary UnOp Expression		-- 
  | Binary BinOp Expression Expression
  deriving (Show)

-- Added
data Term 
  = Factor
  | Term BinOp Factor

data Factor
  = Var Identifier
  | IntConst Integer
  | "(" Expression ")"


data UnOp = Neg
  deriving (Show)

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  deriving (Show)




test1 = Binary Times (Binary Plus (Var "x") (IntConst 5)) (IntConst 7)
test2 = Unary Neg (Var "x")
test3 = Binary Plus (Binary Divide (Var "x") (IntConst 7)) (Var "y")
test4 = Binary Plus (Binary Divide (IntConst 8) (IntConst 3)) (IntConst 6)

showBinSymb :: BinOp -> String
showBinSymb Plus = “+”
showBinSymb Minus = “-”
showBinSymb Times = “*”
showBinSymb Divide = “/”
showBinSymb Mod = “%”

Unparse :: Expresion -> String
Unparse (var id) = id
Uparse (IntConst val) = show val
Uparse (Unary op exp) = show op ++ unparse exp
Unparse (Binary op exp1 exp2) = “(” unparse exp1 ++ showBinSymb op ++ unparse exp2 ++ “)”

Eval :: Store -> Expression -> Integer
Eval store (var id) = no idea
Eval store (IntConst val) = val
Eval store (Unary op exp) = 
    interpretUnOp op eval exp1
Eval store (Binary op exp1 exp2) = 
    interpretUnOp op eval exp1 eval exp2
{--

x = 1;
while x > 0 {
  println x;
  x = x - 1
}

--}

example :: Program
example = [ Assignment "x" (IntConst 10),
            While (Test (Greater (Var "x") (IntConst 0))
              (Block
                [ Println (Var "x"),
                  Assignment "x" (BinExpression Minus (Var "x") (IntConst 1))
                ]
              )
          ]