type COp = String
type CType = String
type CId = String
type Env = [(CId, (CType, Int) )]

data CExpr = CEInt Int
--			 | CEBool Bool
--			 | CEString String
			 | CEId CId
			 | CConst CId CExpr --TODO: exprs at the end
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

store_instr :: String -> String
store_instr "int" = "istore "
store_instr _ = "astore "

load_instr :: String -> String
load_instr "int" = "iload "
load_instr _ = "aload "

get_type :: String -> Env -> String
get_type id1 env = fst (find id1 env)

get_local_var :: String -> Env -> String
get_local_var id1 env = show (snd (find id1 env))

start_env :: Env
start_env = [("", ("", -1) )]

track_stack :: CType -> Env -> Env
track_stack type1 env1 = (drop 1 env1) ++ [("", (type1, -1) )]

compile_str :: Env ->CExpr -> String
compile_str env expr = snd (compile env expr)

compile :: Env -> CExpr -> (Env, String)
compile env (CEInt i1) = ( (track_stack "int" env), "sipush " ++ show i1 ++ "\n")
compile env (CEId id1) = ( (track_stack type1 env), (load_instr type1 ) ++ (get_local_var id1 env) ++ "\n")
								where type1 = get_type id1 env
compile env (CConst id1 e1) = (env, (new_adt id1 (compile_str env e1) ) )
--compile (CEBool b1) = show b1
--compile (CEString str1) = str1

compile env (CEOp e1 op1 e2) = (env, (compile_str env e1) ++ (compile_str env e2) ++ (op_func op1) ++ "\n")

compile env (CENewVar id1 t1 e1) =  (env', (compile_str env e1) ++ (store_instr t1) ++ show ((length env) + 1) ++ "\n")
														where env' = [(id1, (t1, (length env) + 1) )] ++ env

compile env (CExprs [e1]) = (fst compiled, snd compiled)
								where 
									compiled = compile env e1
compile env (CExprs (e1:es)) = ( (fst compiled), (snd res1) ++ (snd  compiled) )
								where 
									compiled = (compile (fst res1) (CExprs es) )
									res1 = (compile env e1)

-- creates new object of ADT class
-- gets tag and jvm code responsible for creating value
-- after this function new object is on the top of the stack
new_adt :: String -> String -> String
new_adt tag value_str = "new Adt\n"
	      				++ "dup\n"
		  				++ "invokespecial Adt/<init>()V\n"
		  				++ "astore_1\n"
		  				++ "aload_1\n"
		  				++ "ldc \"" ++ tag ++ "\"\n"
		  				++ "putfield Adt/tag Ljava/lang/String;\n"
		  				++ "aload_1\n"
		  				++ value_str
		  				++ "putfield Adt/value I\n"
		  				++ "aload_1\n"


adt_class :: String
adt_class = ".class public Adt\n.super java/lang/Object\n\n"
			++ ".field public tag Ljava/lang/String;\n"
			++ ".field public value I\n"
			++ ".field public arr [LAdt;\n"
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
jasminWrapper str1 = preamble_main ++ static_main_start ++ str1 ++ static_main_end

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

--testing constructor
--sth:: Age = Age 3; sth
test6 = (CExprs [(CENewVar "sth" "Age"  (CConst "Age" (CEInt 3) ) ), (CEInt 2)] )

main = do
		writeFile "adt.j" adt_class
		putStrLn (jasminWrapper ((snd (compile start_env test6) )))



find id env = case lookup id env of
		   Just e -> e


