%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}


%token EOF
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_BOOL
%token TYPE_POS
%token TYPE_COLOR
%token TYPE_POINT
%token TYPE_LIST
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token AND
%token OR
%token EQ
%token NE
%token LT
%token GT
%token LE
%token GE
%token USUB
%token NOT
%token Head
%token TAIL
%token FLOOR
%token HEAD
%token FLOAT_OF_INT
%token COS
%token SIN
%token SEMICOLON
%token COMMA
%token L_PAR
%token R_PAR
%token L_CUR_BRK
%token R_CUR_BRK
%token L_SQ_BRK
%token R_SQ_BRK
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
%token <string> STRING
%left AND OR
%left EQ NEQ LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%nonassoc NOT


%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }
| LT a = argument_list GT e = expression EOF {Program(a,Block([],Annotation.create $loc))}
| e = expression EOF { Program([],Block([e],Annotation.create $loc)) }


expression:
| {Nop}
/*
| i=INT { Constant_i(i,Annotation.create $loc) }
| f=FLOAT { Constant_f(f,Annotation.create $loc) }
| b=BOOL { Constant_b(b,Annotation.create $loc) }
| s=STRING { String(s,Annotation.create $loc) }
| id=ID { Variable(id,Annotation.create $loc) }

| e1 = expression op = binop e2 = expression { Binary_operator(op,e1,e2,Annotation.create $loc) }
| op = unop e = expression { Unary_operator(op,e,Annotation.create $loc) }
*/
types:
| TYPE_INT {Type_int} 
| TYPE_FLOAT{Type_float} 
| TYPE_BOOL {Type_bool }
| TYPE_POS {Type_pos}
| TYPE_COLOR {Type_color}
| TYPE_POINT {Type_point}

argument_list:
| t = argument_list SEMICOLON arg = argument {arg :: t}
| arg = argument {arg :: []}
| {[]}

argument:
| t = types L_PAR name = ID R_PAR {Argument(name, t,Annotation.create $loc)}
| TYPE_LIST L_PAR t = types R_PAR L_PAR name = ID R_PAR {Argument(name, Type_list(t), Annotation.create $loc)}


%inline binop:
| ADD   { Add }
| SUB   { Sub }
| MUL   { Mul }
| DIV   { Div }
| MOD   { Mod }
| AND   { And }
| OR  { Or }
| EQ    { Eq }
| LT   { Lt }
| GT    { Gt }

%inline unop:
| USUB  { USub }
| NOT  { Not }
| TAIL  { Tail }
| FLOOR { Floor }
| FLOAT_OF_INT { Float_of_int }
| COS   { Cos }
| SIN   { Sin }
