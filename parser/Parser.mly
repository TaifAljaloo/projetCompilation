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
%token BEGIN
%token END
%token NOP
%token DRAW
%token FOREACH
%token IN
%token COPY
%token FOR
%token FROM
%token TO
%token STEP
%left AND OR
%left EQ NEQ LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%nonassoc NOT


%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }
| LT a = argument_list GT e = statement EOF {Program(a,e)}
| e = statement EOF { Program([],e) }

expression:
| i=INT { Constant_i(i,Annotation.create $loc) }
| f=FLOAT { Constant_f(f,Annotation.create $loc) }
| b=BOOL { Constant_b(b,Annotation.create $loc) }
| TYPE_POS L_PAR x = expression COMMA y = expression R_PAR {Pos(x,y,Annotation.create $loc)}
| TYPE_COLOR L_PAR r = expression COMMA g = expression COMMA b = expression R_PAR {Color(r,g,b,Annotation.create $loc)}
| TYPE_POINT L_PAR pos = expression COMMA color = expression R_PAR {Point(pos, color, Annotation.create $loc)}
| id = ID {Variable(id, Annotation.create $loc)}
| e1 = expression op = binop e2 = expression { Binary_operator(op,e1,e2,Annotation.create $loc) }
| op = unop e = expression { Unary_operator(op,e,Annotation.create $loc) }
//Field eccessor??
| L_SQ_BRK el = expression_list R_SQ_BRK {List(el, Annotation.create $loc)}
//Cons?

types:
| TYPE_INT {Type_int} 
| TYPE_FLOAT{Type_float} 
| TYPE_BOOL {Type_bool }
| TYPE_POS {Type_pos}
| TYPE_COLOR {Type_color}
| TYPE_POINT {Type_point}

statement:
| BEGIN s = statement_list END {Block(s, Annotation.create $loc)}
| COPY L_PAR e1 = expression COMMA e2 = expression R_PAR {Assignment(e1, e2, Annotation.create $loc)} // Assignment
| t = types L_PAR id = ID R_PAR {Variable_declaration(id, t, Annotation.create $loc)} // VarDecl
| BEGIN s = statement_list END {Block(s, Annotation.create $loc)} // Block
//| //IfThenElse
| FOR id=ID FROM init = expression TO target = expression STEP step = expression body = statement SEMICOLON {For(id, init, target, step, body, Annotation.create $loc)}//For
| FOREACH id = ID IN e = expression s = statement {Foreach(id, e, s, Annotation.create $loc)} //For each
| DRAW L_PAR e = expression R_PAR {Draw(e, Annotation.create $loc)} //Draw
| NOP {Nop} //Nop
//| //Print

argument:
| t = types L_PAR name = ID R_PAR {Argument(name, t,Annotation.create $loc)}
| TYPE_LIST L_PAR t = types R_PAR L_PAR name = ID R_PAR {Argument(name, Type_list(t), Annotation.create $loc)}


argument_list:
| t = argument_list SEMICOLON arg = argument {arg :: t}
| arg = argument {arg :: []}
| {[]}

statement_list:
| t = statement_list SEMICOLON st = statement {st::t}
| st = statement {st::[]}
| {[]}

expression_list:
| t = expression_list COMMA e = expression {e :: t}
| e = expression {e::[]}
| {[]}

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
