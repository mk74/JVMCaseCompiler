type COp = String

data CExpr = CEInt Int
			 | CEBool Bool
			 | CEString String
			 | CEop CExpr COp CExpr

stack_load :: CExpr -> CExpr -> String
stack_load (CEInt i1) (CEInt i2) = "sipush " ++ show i1 ++ "\nistore_1\n" ++ "sipush " ++ show i2 ++ "\nistore_2\n" ++"iload_1\niload_2\n"

op_func :: String -> String
op_func "+" = "iadd"
op_func "-" = "isub"
op_func "*" = "imul"
op_func "/" = "idiv"

compile :: CExpr -> String
compile (CEInt i1) = show i1
compile (CEBool b1) = show b1
compile (CEString str1) = str1

compile (CEop e1 op1 e2) = (stack_load e1 e2) ++ (op_func op1)

{-
compile (CEop e1 "<" e2) = "Caciek"
compile (CEop e1 "==" e2) = "Caciek"
compile (CEop e1 "and" e2) = "Caciek"
compile (CEop e1 "or" e2) = "Caciek"-}

--compile _ = "Maciek"

preamble :: String
preamble = ".class public Program.program\n.super java/lang/Object\n\n"
			++ ".method public <init>()V\n"
			++ "aload_0\ninvokenonvirtual java/lang/Object/<init>()V\n"
  			++ "return\n.end method\n\n"

static_main_start :: String
static_main_start = ".method public static main([Ljava/lang/String;)V\n"
  					++ ".limit stack 2\n.limit locals 2\n"

static_main_end :: String
static_main_end = "getstatic     java/lang/System/out Ljava/io/PrintStream;\n"
  				  ++ "ldc           \"Hello World.\"\n"
  				  ++ "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n"
  				  ++ "return\n.end method"

jasminWrapper :: String -> String
jasminWrapper str1 = preamble ++ static_main_start ++ str1 ++ static_main_end

main = putStrLn (jasminWrapper "") --(compile (CEop (CEInt 10) "+" (CEInt 15) ))


