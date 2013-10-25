type COp = String

data CExpr = CEInt Int
			 | CEBool Bool
			 | CEString String
			 | CEop CExpr COp CExpr

compile :: CExpr -> String
compile (CEInt i1) = show i1
compile (CEBool b1) = show b1
compile (CEString str1) = str1
compile _ = "Maciek"

main = print (compile (CEInt 10))