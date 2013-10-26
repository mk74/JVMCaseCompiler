type COp = String
type CType = String
type CId = String
type Env = [(Int, Int)] --maps string id to local variable's number TODO

data CExpr = CEInt Int
--			 | CEBool Bool
--			 | CEString String
			 | CEId Int --TODO: exprs at the end
			 | CEop CExpr COp CExpr
--			 | CENewVar CEId CType CExpr
			 | CENewVar CExpr CType CExpr
			 | CExprs CExpr CExpr

stack_load :: CExpr -> CExpr -> String
stack_load (CEInt i1) (CEInt i2) = "sipush " ++ show i1 ++ "\nsipush " ++ show i2 ++ "\n"

op_func :: String -> String
op_func "+" = "iadd"
op_func "-" = "isub"
op_func "*" = "imul"
op_func "/" = "idiv"

--op_func "<" = 
--op_func "==" = ""
--op_func "and" = "iand"
--op_func "or" = "ior"

compile :: Env -> CExpr -> (Env, String)
compile env (CEInt i1) = (env, show i1)
compile env (CEId i1) = (env, "iload " ++ show (find i1 env) ++ "\n")
--compile (CEBool b1) = show b1
--compile (CEString str1) = str1

compile env (CEop (CEInt i1) op1 (CEInt i2)) = (env, (stack_load (CEInt i1) (CEInt i2)) ++ (op_func op1) ++ "\n")
compile env (CEop e1 op1 e2) = (env, (snd (compile env e1)) ++ (snd (compile env e2)) ++ (op_func op1) ++ "\n")

---- assume that type is int, and assign int TODO
compile env (CENewVar (CEId id1) type1 (CEInt i1)) =  (env', "sipush " ++ show i1 ++ "\nistore " ++ show id1 ++ "\n")
														where env' = [(id1, id1)] ++ env

compile env (CExprs e1 e2) = ((fst res1), (snd res1) ++ (snd (compile (fst res1) e2)) )
								where res1 = (compile env e1)

--(env, (snd (compile env e1)) ++ (snd (compile env e2)) ) --this will require tuple to pass env between instructions

--compile _ = "Maciek"

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
test1 = (CEop (CEInt 10) "+" (CEInt 15))

--testing AND operator
--test2 = (CEop (CEInt 2) "and" (CEInt 1))


--testing nested arithmetic operations
test3 = (CEop (CEop (CEInt 10) "*" (CEInt 5)) "-" (CEop (CEInt 4) "/" (CEInt 2)))


--testing defining new variable and using it
test4 =  (CExprs (CENewVar (CEId 5) "int" (CEInt 10)) (CEId 5))

main = do
		writeFile "adt.j" adt_class
		putStrLn (jasminWrapper ((snd (compile [] test4) )))



find id env = case lookup id env of
		   Just e -> e


