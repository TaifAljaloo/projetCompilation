%{
    open Ast

    (*Le parseur doit générer des Ast.program.
    Pour les annotation, il vous suffit de créer l’annotation avec [Annotation.create $loc] qui sera ce qu’il vous faut dans tous les cas : $loc est un paramètre qui correspond aux positions délimitant la chaîne de caractères parsée par la règle où il est utilisé, ce qui est exactement ce qu’il faut mettre dans l’annotation.*)
%}


%token EOF
%token Type_int
%token Type_float
%token Type_bool
%token Type_pos
%token Type_color
%token Type_point
%token Type_list
%token Add
%token Sub
%token Mul
%token Div
%token Mod
%token And
%token Or
%token Eq
%token Ne
%token Lt
%token Gt
%token Le
%token Ge
%token Usub
%token Not
%token Head
%token Tail
%token Floor
%token Float_of_int
%token Cos
%token Sin
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

%left AND OR
%left EQ NEQ LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%nonassoc NOT


%start <program> main
%%

main:
| EOF { Program([],Block([],Annotation.create $loc)) }

types:
| Type_int { Int }
| Type_float { Float }
| Type_bool { Bool }
| Type_pos { Pos }
| Type_color { Color }
| Type_point { Point }
| Type_list { List }