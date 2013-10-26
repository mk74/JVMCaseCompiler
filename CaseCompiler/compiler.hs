type COp = String
type CType = String
type CEId = String

data CExpr = CEInt Int
--			 | CEBool Bool
--			 | CEString String
			 | CEop CExpr COp CExpr
			 | CENewVar CEId CType CExpr

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

compile :: CExpr -> String
compile (CEInt i1) = show i1
--compile (CEBool b1) = show b1
--compile (CEString str1) = str1

compile (CEop (CEInt i1) op1 (CEInt i2)) = (stack_load (CEInt i1) (CEInt i2)) ++ (op_func op1) ++ "\n"
compile (CEop expr1 op1 expr2) = (compile expr1) ++ (compile expr2) ++ (op_func op1) ++ "\n"

--compile (CENewVar id1 type1 expr1) = 

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

--testing addition two numbers
test1 = (CEop (CEInt 10) "+" (CEInt 15))

--testing AND operator
--test2 = (CEop (CEInt 2) "and" (CEInt 1))


--testing nested arithmetic operations
test3 = (CEop (CEop (CEInt 10) "*" (CEInt 5)) "-" (CEop (CEInt 4) "/" (CEInt 2)))

main = do
		writeFile "adt.j" adt_class
		putStrLn (jasminWrapper (compile test3))


