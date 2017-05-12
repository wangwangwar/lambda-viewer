-- Program that goes with "Learn You a Lambda, a Haskell Tutorial", Chapter 3.

-- Datatype definition for lambda expression.
data Term = Var Var | Lam Var Term | App Term Term 
data Var  = V String

-- Show instances for Var and Term.
instance Show Var where
  show (V s) = s

-- instance Show Term where
--   show (Var v)   = show v
--   show (Lam x e) = "(λ" ++ show x ++ "." ++ show e ++ ")"
--  show (App f e) = "(" ++ show f ++ " " ++ show e ++ ")"

-- 一、如果把上面的 show 函数看作是对 Term 的遍历操作，那它是先序 (pre-order) 还是后序 (post-order)？
-- 你能用另一种方法实现 show 函数吗？

-- 二、上面的 show 函数作为 Pretty Print 其实还不够漂亮，因为括号用的太多了。比如 (λx.(x x))，
-- 考虑到 . 的作用范围是到最右边，可以简写为 λx.x x；而如果是 ((f x) y)，考虑到应用结构是左结合 (left associative)，
-- 可以化简为 f x y。你能实现一个新的 show 函数，让它的结果包含最少的括号吗？
instance Show Term where
  show (Var v)   = show v
  show (Lam x e) = "λ" ++ show x ++ "." ++ show e
  show (App (Lam x e') e) = "(" ++ show (Lam x e') ++ ") " ++ show e
  show (App f (App f' e')) = show f ++ " (" ++ show (App f' e') ++ ")"
  show (App f e) = show f ++ " " ++ show e

main = do 
  print $ show (Lam (V "x") (App (Var (V "x")) (Var (V "x"))))
  print $ show (App (Lam (V "x") (Var (V "x"))) (Var (V "x")))
  print $ show (App (App (Var (V "f")) (Var (V "x"))) (Var (V "y")))
  print $ show (App (Var (V "f")) (App (Var (V "x")) (Var (V "y"))))
  print $ show (App (App (Var (V "f")) (Var (V "x"))) (Lam (V "x") (App (Var (V "x")) (Var (V "x")))))