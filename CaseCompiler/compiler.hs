type COp = String
type CType = String
type CId = String
type Env = [(CId, (CType, Int) )]

data CExpr = CEInt Int
			 | CEBool Bool
			 | CEString String
			 | CEId CId
			 | CConst CId [CExpr]
			 | CEOp CExpr COp CExpr
			 | CENewVar CId CType CExpr
			 | CExprs [CExpr]
			 | CCase CExpr [CAlt]
			 | CFakeTypedef CId CFakeConstr [CFakeConstr]

data CAlt = CAltVal CExpr CExpr
			| CAltADT CType CExpr
data CFakeConstr = CFakeConstr CId [CType]

op_func :: CType -> String -> String
op_func type1 "+" = "iadd"
op_func type1 "-" = "isub"
op_func type1 "*" = "imul"
op_func type1 "/" = "idiv"
op_func "int" "==" = "invokestatic Program/compare_ints(II)I\n"
op_func "string" "==" = "invokestatic Program/compare_strs(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Boolean;\n"
op_func type1 "<" = "invokestatic Program/less_int_than(II)I\n"
op_func type1 "and" = "iand"
op_func type1 "or" = "ior"

store_instr :: String -> String
store_instr "int" = "istore "
store_instr _ = "astore "

load_instr :: String -> String
load_instr "int" = "iload "
load_instr _ = "aload "


--------------------------------------------------------------------------------------------------------
--Env management functions: getting, type, keeping stack trace and depth
---------------------------------------------------------------------------------------------------------
get_type :: String -> Env -> String
get_type id1 env = fst (find id1 env)

get_local_var :: String -> Env -> String
get_local_var id1 env = show (snd (find id1 env))

start_env :: Env
start_env = [("_depth", ("", 0)), ("_stack_trace", ("", -1) )]

track_stack :: CType -> Env -> Env
track_stack type1 env1 = (init env1) ++ [("_stack_trace", (type1, -1) )]

get_stack_trace :: Env -> CType
get_stack_trace env = fst (find "_stack_trace" env)

get_depth :: Env -> Int
get_depth env = snd (find "_depth" env)

increase_depth :: Env -> Env
increase_depth env = update_depth env (old_depth + 1) 
						where
							old_depth = get_depth env

decrease_depth :: Env -> Env
decrease_depth env = update_depth env (old_depth - 1) 
						where
							old_depth = get_depth env

update_depth :: Env -> Int -> Env 
update_depth env i1 = (init (init env)) ++ [("_depth", ("", i1) )] ++ [stack_track]
						where stack_track = (last env)

--------------------------------------------------------------------------------------------------------
--Main compile function
---------------------------------------------------------------------------------------------------------

compile :: Env -> CExpr -> (Env, String)
compile env (CEInt i1) = ( (track_stack "int" env), "sipush " ++ show i1 ++ "\n")
compile env (CEBool True) = ( (track_stack "boolean" env), "iconst_1\n" ++ boolean_value)
compile env (CEBool False) = ( (track_stack "boolean" env), "iconst_0\n" ++ boolean_value)
compile env (CEString str1) = ( (track_stack "string" env), "ldc \"" ++ str1 ++ "\"\n")

compile env (CEId id1) = ( (track_stack type1 env), (load_instr type1 ) ++ (get_local_var id1 env) ++ "\n")
								where type1 = get_type id1 env

compile env (CConst id1 es) = ( (track_stack "object" env), (create_adt_inline id1 0 (length es) ) ++ (loop_add_members env es) )

compile env (CEOp e1 op1 e2) = ( env', (snd compiled) ++ (compile_str env e2) ++ (op_func (get_stack_trace env') op1) ++ "\n")
									where 
										compiled = (compile env e1)
										env' = (fst compiled)

compile env (CENewVar id1 t1 e1) =  (env', (compile_str env e1) ++ (store_instr t1) ++ show ((length env)) ++ "\n")
														where env' = [(id1, (t1, (length env)) )] ++ env

compile env (CExprs [e1]) = (fst compiled, snd compiled)
								where 
									compiled = compile env e1
compile env (CExprs (e1:es)) = ( (fst compiled), (snd res1) ++ (snd  compiled) )
								where 
									compiled = (compile (fst res1) (CExprs es) )
									res1 = (compile env e1)

compile env (CFakeTypedef id1 constr1 constrs) = (env, "")

compile env (CCase e cavs) = ( (fst compiled), (snd compiled) ++ (mult_dup ( (length cavs) -1) ) ++  
							   (loop_cases env' cavs 0) ++ (case_statement_end depth) )
								where 
									compiled = (compile env e)
									depth = get_depth env'
									env' = increase_depth (fst compiled)



loop_cases :: Env -> [CAlt] -> Int -> String
loop_cases env [(CAltVal e_cond e_exec)] i = (compile_str env e_cond) ++ "if_icmpne <default_case_" ++ show depth ++ ">\n" 
											 ++ (compile_str env e_exec) ++ "goto <end_case_" ++ show depth ++ ">\n"
												where depth = get_depth env
loop_cases env ((CAltVal e_cond e_exec):es) i = (compile_str env e_cond) ++ "if_icmpne <case_" ++ show depth ++ "_" ++ show i ++ ">\n" 
													++ (mult_pop (length es) ) ++ (compile_str env e_exec) 
											     ++ "goto <end_case_" ++ show depth ++ ">\n<case_" ++ show depth ++ "_" ++ show i ++ ">:\n" 
											     ++ (loop_cases env es (i+1) )
											  	where depth = get_depth env

case_statement_end :: Int -> String
case_statement_end depth = "<default_case_" ++ show depth ++ ">:\nsipush 1\n<end_case_" ++ show depth ++ ">:\n"

loop_add_members :: Env -> [CExpr] -> String
loop_add_members env [(CEInt i1)] = (create_adt_inline "int" i1 0) ++ add_member_inline
loop_add_members env [e1] = (compile_str env e1) ++ (add_member_inline)
loop_add_members env ((CEInt i1):es) = (create_adt_inline "int" i1 0) ++ add_member_inline ++ (loop_add_members env es)
loop_add_members env (e1:es) = (compile_str env e1) ++ add_member_inline ++ (loop_add_members env es)

create_adt_inline :: String -> Int -> Int -> String
create_adt_inline tag value n = "ldc \"" ++ tag ++ "\"\n"
						  		++ "sipush " ++ show value ++ "\n"
						  		++ "sipush " ++ show n ++ "\n"
						  		++ "invokestatic Adt/create(Ljava/lang/String;II)LAdt;\n"
						  		++ (mult_dup n)

add_member_inline :: String
add_member_inline = "invokevirtual Adt/add(LAdt;)V\n"


printing_code :: Env -> String
printing_code env = (store_instr type1 ) ++ " " ++ new_local_id ++ "\n"
				  	++"getstatic java/lang/System/out Ljava/io/PrintStream;\n"
 				  	++ (load_instr type1 ) ++ " " ++ new_local_id ++ "\n"
  				  	++ "invokevirtual java/io/PrintStream/println(" ++ (println_signature type1) ++ ")V\n"
  				  		where 
  				  			type1 = get_stack_trace env
  				  			new_local_id = show ( (length env))

println_signature :: String -> String
println_signature "int" = "I"
println_signature _ = "Ljava/lang/Object;"


boolean_value ::String
boolean_value = "invokestatic java/lang/Boolean/valueOf(Z)Ljava/lang/Boolean;\n"

adt_class :: String
adt_class = ".class public Adt\n.super java/lang/Object\n\n"
			++ ".field public tag Ljava/lang/String;\n"
			++ ".field public value I\n.field public arr [LAdt;\n"
			++ ".field private index I = 0\n.method public <init>()V\n"
			++ "aload_0\ninvokenonvirtual java/lang/Object/<init>()V\n"
  			++ "return\n.end method\n\n"
 -- String toString()
  			++ ".method public toString()Ljava/lang/String;\n"
			++ "aload_0\ngetfield Adt/tag Ljava/lang/String;\n"
  			++ "areturn\n.end method\n\n"
 -- Adt create(String, int, int)
   			++ ".method public static create(Ljava/lang/String;II)LAdt;\n"
   			++ ".limit stack 10\n.limit locals 10\n"
			++ "new Adt\ndup\ninvokespecial Adt/<init>()V\n"
		  	++ "astore_3\naload_3\naload_0\n"
			++ "putfield Adt/tag Ljava/lang/String;\n"
		  	++ "aload_3\niload_1\nputfield Adt/value I\n"
		  	++ "aload_3\niload 2\nanewarray Adt\n"
		  	++ "putfield Adt/arr [LAdt;\naload_3\n"
  			++ "areturn\n.end method\n\n"
 -- void add(Adt)
			++ ".method public add(LAdt;)V\n"
   			++ ".limit stack 10\n.limit locals 10\n"
   			++ "aload_0\ngetfield Adt/arr [LAdt;\n"
   			++ "aload_0\ngetfield Adt/index I\n"
   			++ "aload_1\naastore\naload_0\ndup\n"
   			++ "getfield Adt/index I\n"
   			++ "iconst_1\niadd\nputfield Adt/index I\n"
   			++ "return\n.end method\n\n"

preamble_main :: String
preamble_main = ".class public Program\n.super java/lang/Object\n\n"
				++ ".method public <init>()V\n"
				++ "aload_0\ninvokenonvirtual java/lang/Object/<init>()V\n"
  				++ "return\n.end method\n\n"

static_main_start :: String
static_main_start = 
-- int compare_ints(int, int)
					".method public static compare_ints(II)I\n"
					++ ".limit stack 3\n.limit locals 3\niload_0\niload_1\n"
					++ "if_icmpne <false_equal_if>\niconst_1\ngoto <end_equal_if>\n<false_equal_if>:\niconst_0\n<end_equal_if>:\n"
					++ "ireturn\n.end method\n\n"
-- Boolean compare_strs(String, String)
					++ ".method public static compare_strs(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Boolean;\n"
					++ ".limit stack 3\n.limit locals 3\n"
					++ "aload_0\naload_1\ninvokevirtual java/lang/String/equals(Ljava/lang/Object;)Z\n"
					++ boolean_value
					++ "areturn\n.end method\n\n"
-- int less_int_than(int, int)
					++ ".method public static less_int_than(II)I\n"
					++ ".limit stack 3\n.limit locals 3\niload_0\niload_1\n"
					++ "if_icmpge <false_less_if>\niconst_1\ngoto <end_less_if>\n<false_less_if>:\niconst_0\n<end_less_if>:\n"
					++ "ireturn\n.end method\n\n\n"
-- public static main(String[] args)
					++ ".method public static main([Ljava/lang/String;)V\n"
  					++ ".limit stack 100\n.limit locals 100\n"

static_main_end :: String
static_main_end = "return\n.end method\n"

jasminWrapper :: String -> String
jasminWrapper prog_code = preamble_main ++ static_main_start ++ prog_code ++ static_main_end

--------------------------------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------------------------------

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
test6 = (CExprs [(CENewVar "sth" "Age"  (CConst "Age" [(CEInt 3)] ) ), (CEId "sth")] )

--testing string variable
--"Something"
test7 = (CEString "Something")

--testing defining new variable as string
--sth:: string = "Something"; sth
test8 = (CExprs [(CENewVar "sth" "string" (CEString "Something") ) , (CEId "sth")] )

--testing defining new variable as boolean (true)
--sth:: bool = True; sth
test9 = (CExprs [(CENewVar "sth" "bool" (CEBool True) ) , (CEId "sth")] )

--testing defining new variable as boolean (true)
--sth:: bool = False; sth
test10 = (CExprs [(CENewVar "sth" "bool" (CEBool False) ) , (CEId "sth")] )

--testing data constructor(recursive data type)
-- Age {Person 10} 10
test11 = (CConst "Age" [ (CConst "Person" [(CEInt 10)] ), (CEInt 10) ] )

--testing == operator
-- 10 == 11
test12 = (CEOp (CEInt 10) "==" (CEInt 11))

--testing < operator
-- 11 < 11
test13 = (CEOp (CEInt 11) "<" (CEInt 11))

--testing simplest case statement
-- case (int 0) of (int 0) -> (int 1)
test14 = (CCase (CEInt 0) [(CAltVal (CEInt 0) (CEInt 1) ) ] )

--testing case statement with many possibilities
--  case (int 0) of (int 1) -> (int 2) | (int 2)-> (int 3) | (int 3)-> (int 4) | (int 4) -> (int 5) | (int 0) -> (int 1)
test15 = (CCase (CEInt 0) [(CAltVal (CEInt 1) (CEInt 2)), (CAltVal (CEInt 2) (CEInt 3)), 
						   (CAltVal (CEInt 3) (CEInt 4)), (CAltVal (CEInt 4) (CEInt 5)), 
						   (CAltVal (CEInt 0) (CEInt 1)) ] )

--testing comparing two strings
-- "sth"=="sth"
test16 = (CEOp (CEString "sth") "==" (CEString "sth"))

--testing nested case statement
-- case (int 0) of
--			(int 1) -> 
--				(case (int 1) of 
--						(int 0) -> (int 1) )
--						(int 1) -> (int 2) ) 
--			(int 0) -> 
--				(case (int 1) of 
--						(int 0) -> (int 1) )
--						(int 1) -> (int 2) )
test17 = (CCase (CEInt 0) [(CAltVal (CEInt 1) 
								(CCase (CEInt 1) 
									[(CAltVal (CEInt 0) (CEInt 1) ), 
									 (CAltVal (CEInt 1) (CEInt 2) ) ] 
								)  
						   ),
							(CAltVal (CEInt 0) 
								(CCase (CEInt 1) 
									[(CAltVal (CEInt 0) (CEInt 1) ), 
									 (CAltVal (CEInt 1) (CEInt 2) ) ] 
								)  
						   )
						  ] 
		  )

--testing defining new variable as boolean (true)
--sth:: Age = Age {Person 10} 10; sth
test18 = (CExprs [(CENewVar "sth" "Age" (CConst "Age" [ (CConst "Person" [(CEInt 10)] ), (CEInt 10) ] )),
					(CEId "sth")
				 ]
		 )


--testing simple ADT-based case statement
--test19 = (CExprs [(CENewVar "sth" "Age" (CConst "Age" [ (CEInt 10), (CEInt 4) ] ) ),
--					(CEInt 3)])

-- testing simple case statement
-- case (int 0) of (int 0)-> (int 1) | (int 1) -> (int 0)
example1 = (CCase (CEInt 0) [(CAltVal (CEInt 0) (CEInt 1) ), (CAltVal (CEInt 1) (CEInt 0) ) ] )

--testing simple typedef/new variable example
-- type Time = Hour int | Min int; t :: Time = Min 2; t }
example2 = (CExprs [(CFakeTypedef "Time" (CFakeConstr "Hour" ["int"] ) [ (CFakeConstr "min" ["int"] ) ]), 
					(CENewVar "t" "Time" ( CConst "Min" [(CEInt 2)] ) ), 
					(CEId "t")])

--------------------------------------------------------------------------------------------------------
--program's main functions
--------------------------------------------------------------------------------------------------------
main = do
		writeFile "adt.j" adt_class
		putStrLn (jasminWrapper (snd compiled ++ printing_code (fst compiled) ) )
			where compiled = (compile start_env test18)


--------------------------------------------------------------------------------------------------------
--helper functions/wrappers
---------------------------------------------------------------------------------------------------------
find id env = case lookup id env of
		   Just e -> e

mult_dup :: Int -> String
mult_dup n = (concat ( replicate n "dup\n" ) )

mult_pop :: Int -> String
mult_pop n = (concat ( replicate n "pop\n" ) )

compile_str :: Env ->CExpr -> String
compile_str env expr = snd (compile env expr)


