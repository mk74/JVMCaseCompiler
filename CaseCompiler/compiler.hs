type COp = String
type CType = String
type CId = String
type Env = [(CId, Int)] --maps string id to local variable's number TODO

data CExpr = CEInt Int
--			 | CEBool Bool
--			 | CEString String
			 | CEId CId --TODO: exprs at the end
			 | CEOp CExpr COp CExpr
			 | CENewVar CId CType CExpr
			 | CExprs [CExpr]

op_func :: String -> String
op_func "+" = "iadd"
op_func "-" = "isub"
op_func "*" = "imul"
op_func "/" = "idiv"

--op_func "<" = 
--op_func "==" = ""
--op_func "and" = "iand"
--op_func "or" = "ior"

compile_str :: Env ->CExpr -> String
compile_str env expr = snd (compile env expr)

compile :: Env -> CExpr -> (Env, String)
compile env (CEInt i1) = (env, "sipush " ++ show i1 ++ "\n")
compile env (CEId i1) = (env, "iload " ++ show (find i1 env) ++ "\n")
--compile (CEBool b1) = show b1
--compile (CEString str1) = str1

compile env (CEOp e1 op1 e2) = (env, (compile_str env e1) ++ (compile_str env e2) ++ (op_func op1) ++ "\n")

compile env (CENewVar id1 type1 e1) =  (env', (compile_str env e1) ++ "istore " ++ show ((length env) +1) ++ "\n")
														where env' = [(id1, (length env) +1)] ++ env

compile env (CExprs [e1]) = (env, compile_str env e1)
compile env (CExprs (e1:es)) = ( (fst res1), (snd res1) ++ (snd (compile (fst res1) (CExprs es) ) ) )
								where res1 = (compile env e1)

new_adt :: String
new_adt = "new Adt\n"
	      ++ "dup\n"
		  ++ "invokespecial Adt/<init>()V\n"

adt_class :: String
adt_class = ".class public Adt\n.super java/lang/Object\n\n"
			++ ".field public tag Ljava/lang/String;\n"
			++ ".field public value I\n"
--			++ ".field public arr [Ljava/lang/String\n"
			++ ".method public <init>()V\n"
			++ "aload_0\ninvokenonvirtual java/lang/Object/<init>()V\n"
  			++ "return\n.end method\n\n"

preamble_main :: String
preamble_main = ".class public Program\n.super java/lang/Object\n\n"
			++ ".method public <init>()V\n"
			++ "aload_0\ninvokenonvirtual java/lang/Object/<init>()V\n"
  			++ "return\n.end method\n\n"

static_main_start :: String
static_main_start = ".method public static main([Ljava/lang/String;)V\n"
  					++ ".limit stack 100\n.limit locals 100\n"

static_main_end :: String
static_main_end = "istore 99\n"
				  ++"getstatic     java/lang/System/out Ljava/io/PrintStream;\n"
 				  ++ "iload 99\n"
  				  ++ "invokevirtual java/io/PrintStream/println(I)V\n"
  				  ++ "return\n.end method"

jasminWrapper :: String -> String
jasminWrapper str1 = preamble_main ++ static_main_start ++ new_adt ++ str1 ++ static_main_end

test0 = (CEInt 5)

--testing addition two numbers
-- 10 + 15
test1 = (CEOp (CEInt 10) "+" (CEInt 15))

--testing AND operator
test2 = (CEOp (CEInt 2) "and" (CEInt 1))


--testing nested arithmetic operations
-- (10 * 5) - (4/2)
test3 = (CEOp (CEOp (CEInt 10) "*" (CEInt 5)) "-" (CEOp (CEInt 4) "/" (CEInt 2)))


--testing defining new variable and using it
-- sth ::  int = 10 * 5; sth
test4 =  (CExprs [(CENewVar "sth" "int" (CEOp (CEInt 10) "*" (CEInt 5))), (CEId "sth")])


--testing defining new variable and using it
-- sth :: int = 10; sth2 :: int = sth * 2; sth2
test5 =  (CExprs [(CENewVar "sth" "int" (CEInt 10)), (CENewVar "sth2" "int" (CEOp (CEId "sth") "*" (CEInt 2))), (CEId "sth2") ] )

main = do
		writeFile "adt.j" adt_class
		putStrLn (jasminWrapper ((snd (compile [] test5) )))



find id env = case lookup id env of
		   Just e -> e


