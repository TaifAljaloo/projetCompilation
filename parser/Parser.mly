%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}


%token EOF
%token TYPE_INT
%token TYPE_FLOAT
%token TYPE_BOOL
%token TYPE_POINT
%token TYPE_LIST
%token POS
%token COLOR
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
%token NOT
%token TAIL
%token FLOOR
%token HEAD
%token CONCAT
%token FLOAT_OF_INT
%token COS
%token SIN
%token SEMICOLON
%token COMMA
%token L_PAR
%token R_PAR
%token L_SQ_BRK
%token R_SQ_BRK
%token IF
%token ELSE
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
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
%token PI
%token PRINT
%token X_ACCESSOR 
%token Y_ACCESSOR 
%token RED_ACCESSOR
%token GREEN_ACCESSOR
%token BLUE_ACCESSOR
%token DOT
%token CONS
%token TRUE
%token FALSE
%token WHILE
%token TERNARY
%token COLON
%token POW
(*handle Ternary and Colon priority*)
%left TERNARY
%left COLON

%nonassoc IF NOT DOT
%nonassoc ELSE 
%left AND OR EQ NE LT GT LE GE ADD SUB POW MUL DIV MOD 
%right CONCAT


%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }
| e = statement EOF { Program([],e) }
| LT a = argument_list GT e = statement EOF {Program(a,e)}



argument_list:
| t = argument_list SEMICOLON arg = argument {t@[arg]}
| arg = argument {arg :: []}
| {[]}

statement_list:
| t = statement_list SEMICOLON st = statement {t@[st]}
| st = statement {st::[]}
| {[]}

expression_list:
| t = expression_list COMMA e = expression {t @ [e]}
| e = expression {e::[]}
| {[]}

expression:
| i=INT { Constant_i(i,Annotation.create $loc) }
| f=FLOAT { Constant_f(f,Annotation.create $loc) }
| b=BOOL { Constant_b(b,Annotation.create $loc) }
| POS L_PAR x = expression COMMA y = expression R_PAR {Pos(x,y,Annotation.create $loc)}
| COLOR L_PAR r = expression COMMA g = expression COMMA b = expression R_PAR {Color(r,g,b,Annotation.create $loc)}
| TYPE_POINT L_PAR pos = expression COMMA color = expression R_PAR {Point(pos, color, Annotation.create $loc)}
| id = ID {Variable(id, Annotation.create $loc)}
| e1 = expression op = binop e2 = expression { Binary_operator(op,e1,e2,Annotation.create $loc) }
| e1 = expression CONCAT e2 = expression {Cons(e1, e2, Annotation.create $loc)}
| op = unop e = expression { Unary_operator(op,e,Annotation.create $loc) }
| op = unop_par L_PAR e = expression R_PAR { Unary_operator(op,e,Annotation.create $loc) }
| e = expression DOT a=accessor {Field_accessor(a,e,Annotation.create $loc)}
| L_SQ_BRK el = expression_list R_SQ_BRK {List(el, Annotation.create $loc)}
| CONS L_PAR e1 = expression COMMA e2 = expression R_PAR {Cons(e1, e2, Annotation.create $loc)}
| L_PAR e = expression R_PAR {e}
| TRUE {Constant_b(true, Annotation.create $loc)}
| FALSE {Constant_b(false, Annotation.create $loc)}
| PI { Constant_f(3.141592653589,Annotation.create $loc) }
| e1 = expression TERNARY e2 = expression COLON e3 = expression {Ternary_operator(e1, e2, e3, Annotation.create $loc)}


statement:
| COPY L_PAR e1 = expression COMMA e2 = expression R_PAR {Assignment(e1, e2, Annotation.create $loc)} 
| t = types L_PAR id = ID R_PAR {Variable_declaration(id, t, Annotation.create $loc)}
| TYPE_LIST L_PAR t = types R_PAR L_PAR id = ID R_PAR {Variable_declaration(id, Type_list(t), Annotation.create $loc)}
| t = types L_PAR id = ID COMMA value = expression R_PAR {Init_Variable_declaration(id, t, value, Annotation.create $loc)}
| BEGIN s = statement_list END {Block(s, Annotation.create $loc)} 
| IF e = expression s = statement ELSE s2 = statement {IfThenElse(e, s, s2, Annotation.create $loc)}
| IF e = expression s = statement {IfThenElse(e, s, Nop, Annotation.create $loc)}
| FOR id=ID FROM init = expression TO target = expression STEP step = expression body = statement {For(id, init, target, step, body, Annotation.create $loc)}//For
| FOREACH id = ID IN e = expression s = statement {Foreach(id, e, s, Annotation.create $loc)} 
| DRAW L_PAR e = expression R_PAR {Draw(e, Annotation.create $loc)} 
| NOP {Nop} //Nop
| PRINT L_PAR e = expression R_PAR {Print(e, Annotation.create $loc)} 
| WHILE e = expression s = statement {While(e, s, Annotation.create $loc)}

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
| OR    { Or }
| EQ    { Eq }
| NE    { Ne }
| LT    { Lt }
| GT    { Gt }
| LE    { Le }
| GE    { Ge }
| POW { Pow }

%inline unop:
| SUB  { USub }
| NOT  { Not }


%inline unop_par:
| FLOOR { Floor }
| FLOAT_OF_INT { Float_of_int }
| COS   { Cos }
| SIN   { Sin }
| TAIL  { Tail }
| HEAD  { Head }

types:
| TYPE_INT {Type_int} 
| TYPE_FLOAT{Type_float} 
| TYPE_BOOL {Type_bool }
| POS {Type_pos}
| COLOR {Type_color}
| TYPE_POINT {Type_point}

accessor:
| X_ACCESSOR        {X_accessor}
| Y_ACCESSOR        {Y_accessor}
| POS      {Position_accessor}
| COLOR      {Color_accessor}
| RED_ACCESSOR      {Red_accessor}
| GREEN_ACCESSOR    {Green_accessor}
| BLUE_ACCESSOR     {Blue_accessor}