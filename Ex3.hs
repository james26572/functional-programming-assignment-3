module Ex3 where

--required for all Qs:
data CExpr -- the expression datatype
  = Number Float -- floating-point Number
  | Variable String -- variable/identifier name
  | Divide CExpr CExpr -- divide first by second
  | Minus CExpr CExpr -- subtracts second from first
  | Absolute CExpr -- absolute Number
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not CExpr -- logical not
  | LessThan CExpr CExpr -- True if first is less than second
  | IsNull CExpr -- True if numeric Number is zero
  deriving (Eq,Ord,Show)

type Dict = [(String,Float)]
insert :: String -> Float -> Dict -> Dict
insert s f d = (s,f):d
find :: String -> Dict -> Maybe Float
find _ [] = Nothing
find s ((t,f):d)
  | s==t       =  Just f
  | otherwise  =  find s d

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which may have runtime errors):
eval :: Dict -> CExpr -> Float
eval _ (Number x) = x
eval dict (Variable name) = case find name dict of
  Just val -> val
  Nothing -> error ("Variable : " ++ name ++ " not found in the dictionary.")
eval dict (Divide expr1 expr2) =
  let val1 = eval dict expr1
      val2 = eval dict expr2
  in if val2 == 0.0
        then error "Can't divide by zero"
        else val1 / val2
eval dict (Minus expr1 expr2) = eval dict expr1 - eval dict expr2
eval dict (Absolute expr) = abs (eval dict expr)
eval dict (Not expr) = if eval dict expr == 0.0 then 1.0 else 0.0
eval dict (LessThan expr1 expr2) = if eval dict expr1 < eval dict expr2 then 1.0 else 0.0
eval dict (IsNull expr) = if eval dict expr == 0.0 then 1.0 else 0.0

-- Q2 (8 marks)
-- implement the following function (which always returns a Number):
meval :: Dict -> CExpr -> Maybe Float
meval _ (Number x) = Just x
meval dict (Variable name) = find name dict
meval dict (Divide expr1 expr2) = do
  val1 <- meval dict expr1
  val2 <- meval dict expr2
  if val2 == 0.0
    then Nothing
    else Just (val1 / val2)
meval dict (Minus expr1 expr2) = do
  val1 <- meval dict expr1
  val2 <- meval dict expr2
  return (val1 - val2)
meval dict (Absolute expr) = abs <$> meval dict expr
meval dict (Not expr) = do
  val <- meval dict expr
  return (if val /= 0.0 then 0.0 else 1.0)
meval dict (LessThan expr1 expr2) = do
  val1 <- meval dict expr1
  val2 <- meval dict expr2
  return (if val1 < val2 then 1.0 else 0.0)
meval dict (IsNull expr) = do
  val <- meval dict expr
  return (if val == 0.0 then 1.0 else 0.0)

-- Q3 (4 marks)
-- Laws of Arithmetic for this question:
--    x + 0 = x
--    0 + x = x
--    x - 0 = x
--    x - x = 0
--    x * 0 = 0
--    1 * x = x
-- The following function should implement the two laws applicable
-- for *your* CExpr datatype.
simp :: CExpr -> CExpr
simp (Divide (Number 0.0) _) = Number 0.0
simp (Divide expr (Number 1.0)) = simp expr
simp (Divide exp1 exp2) =
  if simp exp1 == simp exp2
    then Number 1.0
    else Divide (simp exp1) (simp exp2)
simp (Minus expr (Number 0.0)) = simp expr
simp (Minus exp1 exp2) =
  if simp exp1 == simp exp2
    then Number 0.0
    else Minus (simp exp1) (simp exp2)
simp (Absolute expr) = Absolute (simp expr)
simp (LessThan exp1 exp2) = LessThan (simp exp1) (simp exp2)
simp (Not e) = Not (simp e)
simp (IsNull (Number 0.0)) = Number 1.0
simp (IsNull _) = Number 0.0
simp expr = expr
-- add extra material below here
-- e.g.,  helper functions, test Numbers, etc. ...

