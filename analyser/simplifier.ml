(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (point, position et couleur) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)
open Ast

let rec simplifier_expr expr =
  match expr with
  | Constant_i (i,annotation) -> Constant_i (i,annotation)
  | Constant_f (f,annotation) -> Constant_f (f,annotation)
  | Constant_b (b,annotation) -> Constant_b (b,annotation)
  | Pos(x,y,annotation) -> Pos(simplifier_expr x, simplifier_expr y, annotation)
  | Point(x,y,annotation) -> Point(simplifier_expr x, simplifier_expr y, annotation)
  | Color(r,g,b,annotation) -> Color(simplifier_expr r, simplifier_expr g, simplifier_expr b, annotation)
  | Variable (s,annotation) -> Variable (s,annotation)
  | Binary_operator(op, e1, e2, anno) -> let e1, e2 = simplifier_expr(e1), simplifier_expr(e2) in (
    match op, e1, e2 with
    | Add, Constant_i(i1, _), Constant_i(i2, _) -> Constant_i(i1 + i2, anno)
    | Add, Constant_f(f1, _), Constant_f(f2, _) -> Constant_f(f1 +. f2, anno)
    | Add, Constant_i(i1, _), Constant_f(f2, _) -> Constant_f(float_of_int i1 +. f2, anno)
    | Add, Constant_f(f1, _), Constant_i(i2, _) -> Constant_f(f1 +. float_of_int i2, anno)
    | Add, Pos(x1, y1, _), Pos(x2, y2, _) -> Pos(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Add, Point(x1, y1, _), Point(x2, y2, _) -> Point(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Add, Color(r1, g1, b1, _), Color(r2, g2, b2, _) -> Color(simplifier_expr(Binary_operator(op, r1, r2, anno)), simplifier_expr(Binary_operator(op, g1, g2, anno)), simplifier_expr(Binary_operator(op, b1, b2, anno)), anno)
    | Sub, Constant_i(i1, _), Constant_i(i2, _) -> Constant_i(i1 - i2, anno)
    | Sub, Constant_f(f1, _), Constant_f(f2, _) -> Constant_f(f1 -. f2, anno)
    | Sub, Constant_i(i1, _), Constant_f(f2, _) -> Constant_f(float_of_int i1 -. f2, anno)
    | Sub, Constant_f(f1, _), Constant_i(i2, _) -> Constant_f(f1 -. float_of_int i2, anno)
    | Sub, Pos(x1, y1, _), Pos(x2, y2, _) -> Pos(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Sub, Point(x1, y1, _), Point(x2, y2, _) -> Point(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Sub, Color(r1, g1, b1, _), Color(r2, g2, b2, _) -> Color(simplifier_expr(Binary_operator(op, r1, r2, anno)), simplifier_expr(Binary_operator(op, g1, g2, anno)), simplifier_expr(Binary_operator(op, b1, b2, anno)), anno)
    | Mul, Constant_i(i1, _), Constant_i(i2, _) -> Constant_i(i1 * i2, anno)
    | Mul, Constant_f(f1, _), Constant_f(f2, _) -> Constant_f(f1 *. f2, anno)
    | Mul, Constant_i(i1, _), Constant_f(f2, _) -> Constant_f(float_of_int i1 *. f2, anno)
    | Mul, Constant_f(f1, _), Constant_i(i2, _) -> Constant_f(f1 *. float_of_int i2, anno)
    | Mul, Pos(x1, y1, _), Pos(x2, y2, _) -> Pos(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Mul, Point(x1, y1, _), Point(x2, y2, _) -> Point(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Mul, Color(r1, g1, b1, _), Color(r2, g2, b2, _) -> Color(simplifier_expr(Binary_operator(op, r1, r2, anno)), simplifier_expr(Binary_operator(op, g1, g2, anno)), simplifier_expr(Binary_operator(op, b1, b2, anno)), anno)
    | Div, Constant_i(i1, _), Constant_i(i2, _) -> Constant_i(i1 / i2, anno)
    | Div, Constant_f(f1, _), Constant_f(f2, _) -> Constant_f(f1 /. f2, anno)
    | Div, Constant_i(i1, _), Constant_f(f2, _) -> Constant_f(float_of_int i1 /. f2, anno)
    | Div, Constant_f(f1, _), Constant_i(i2, _) -> Constant_f(f1 /. float_of_int i2, anno)
    | Div, Pos(x1, y1, _), Pos(x2, y2, _) -> Pos(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Div, Point(x1, y1, _), Point(x2, y2, _) -> Point(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Div, Color(r1, g1, b1, _), Color(r2, g2, b2, _) -> Color(simplifier_expr(Binary_operator(op, r1, r2, anno)), simplifier_expr(Binary_operator(op, g1, g2, anno)), simplifier_expr(Binary_operator(op, b1, b2, anno)), anno)
    | Mod, Constant_i(i1, _), Constant_i(i2, _) -> Constant_i(i1 mod i2, anno)
    | Mod, Constant_f(f1, _), Constant_f(f2, _) -> Constant_f(mod_float f1 f2, anno)
    | Mod, Constant_i(i1, _), Constant_f(f2, _) -> Constant_f(mod_float (float_of_int i1) f2, anno)
    | Mod, Constant_f(f1, _), Constant_i(i2, _) -> Constant_f(mod_float f1 (float_of_int i2), anno)
    | Mod, Pos(x1, y1, _), Pos(x2, y2, _) -> Pos(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Mod, Point(x1, y1, _), Point(x2, y2, _) -> Point(simplifier_expr(Binary_operator(op, x1, x2, anno)), simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Mod, Color(r1, g1, b1, _), Color(r2, g2, b2, _) -> Color(simplifier_expr(Binary_operator(op, r1, r2, anno)), simplifier_expr(Binary_operator(op, g1, g2, anno)), simplifier_expr(Binary_operator(op, b1, b2, anno)), anno)
    | And, Constant_b(b1, _), Constant_b(b2, _) -> Constant_b(b1 && b2, anno)
    | Or, Constant_b(b1, _), Constant_b(b2, _) -> Constant_b(b1 || b2, anno)
    | Eq, Constant_i(i1, _), Constant_i(i2, _) -> Constant_b(i1 = i2, anno)
    | Eq, Constant_f(f1, _), Constant_f(f2, _) -> Constant_b(f1 = f2, anno)
    | Eq, Constant_i(i1, _), Constant_f(f2, _) -> Constant_b(float_of_int i1 = f2, anno)
    | Eq, Constant_f(f1, _), Constant_i(i2, _) -> Constant_b(f1 = float_of_int i2, anno)
    | Eq, Constant_b(b1, _), Constant_b(b2, _) -> Constant_b(b1 = b2, anno)
    | Eq, Pos(x1, y1, _), Pos(x2, y2, _) -> Constant_b(simplifier_expr(Binary_operator(op, x1, x2, anno)) = simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Eq, Point(x1, y1, _), Point(x2, y2, _) -> Constant_b(simplifier_expr(Binary_operator(op, x1, x2, anno)) = simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Eq, Color(r1, g1, b1, _), Color(r2, g2, b2, _) -> Constant_b(simplifier_expr(Binary_operator(op, r1, r2, anno)) = simplifier_expr(Binary_operator(op, g1, g2, anno)) && simplifier_expr(Binary_operator(op, g1, g2, anno)) = simplifier_expr(Binary_operator(op, b1, b2, anno)), anno)
    | Ne, Constant_i(i1, _), Constant_i(i2, _) -> Constant_b(i1 <> i2, anno)
    | Ne, Constant_f(f1, _), Constant_f(f2, _) -> Constant_b(f1 <> f2, anno)
    | Ne, Constant_i(i1, _), Constant_f(f2, _) -> Constant_b(float_of_int i1 <> f2, anno)
    | Ne, Constant_f(f1, _), Constant_i(i2, _) -> Constant_b(f1 <> float_of_int i2, anno)
    | Ne, Constant_b(b1, _), Constant_b(b2, _) -> Constant_b(b1 <> b2, anno)
    | Ne, Pos(x1, y1, _), Pos(x2, y2, _) -> Constant_b(simplifier_expr(Binary_operator(op, x1, x2, anno)) <> simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Ne, Point(x1, y1, _), Point(x2, y2, _) -> Constant_b(simplifier_expr(Binary_operator(op, x1, x2, anno)) <> simplifier_expr(Binary_operator(op, y1, y2, anno)), anno)
    | Ne, Color(r1, g1, b1, _), Color(r2, g2, b2, _) -> Constant_b(simplifier_expr(Binary_operator(op, r1, r2, anno)) <> simplifier_expr(Binary_operator(op, g1, g2, anno)) || simplifier_expr(Binary_operator(op, g1, g2, anno)) <> simplifier_expr(Binary_operator(op, b1, b2, anno)), anno)
    | Lt, Constant_i(i1, _), Constant_i(i2, _) -> Constant_b(i1 < i2, anno)
    | Lt, Constant_f(f1, _), Constant_f(f2, _) -> Constant_b(f1 < f2, anno)
    | Lt, Constant_i(i1, _), Constant_f(f2, _) -> Constant_b(float_of_int i1 < f2, anno)
    | Lt, Constant_f(f1, _), Constant_i(i2, _) -> Constant_b(f1 < float_of_int i2, anno)
    | Gt, Constant_i(i1, _), Constant_i(i2, _) -> Constant_b(i1 > i2, anno)
    | Gt, Constant_f(f1, _), Constant_f(f2, _) -> Constant_b(f1 > f2, anno)
    | Gt, Constant_i(i1, _), Constant_f(f2, _) -> Constant_b(float_of_int i1 > f2, anno)
    | Gt, Constant_f(f1, _), Constant_i(i2, _) -> Constant_b(f1 > float_of_int i2, anno)
    | Ge, Constant_i(i1, _), Constant_i(i2, _) -> Constant_b(i1 >= i2, anno)
    | Ge, Constant_f(f1, _), Constant_f(f2, _) -> Constant_b(f1 >= f2, anno)
    | Ge, Constant_i(i1, _), Constant_f(f2, _) -> Constant_b(float_of_int i1 >= f2, anno)
    | Ge, Constant_f(f1, _), Constant_i(i2, _) -> Constant_b(f1 >= float_of_int i2, anno)
    | Le, Constant_i(i1, _), Constant_i(i2, _) -> Constant_b(i1 <= i2, anno)
    | Le, Constant_f(f1, _), Constant_f(f2, _) -> Constant_b(f1 <= f2, anno)
    | Le, Constant_i(i1, _), Constant_f(f2, _) -> Constant_b(float_of_int i1 <= f2, anno)
    | Le, Constant_f(f1, _), Constant_i(i2, _) -> Constant_b(f1 <= float_of_int i2, anno)
    | Pow, Constant_i(i1, _), Constant_i(i2, _) -> Constant_i(int_of_float ((float_of_int i1) ** (float_of_int i2)), anno)
    | Pow, Constant_f(f1, _), Constant_f(f2, _) -> Constant_f(f1 ** f2, anno)
    | _ -> Binary_operator(op, e1, e2, anno)
    )
  | Unary_operator (op, expr, annotation) -> (
    let expr' = simplifier_expr expr in
    match op, expr' with
    | Not, Constant_b(b, _) -> Constant_b(not b, annotation)
    | USub, Constant_i(i, _) -> Constant_i(-i, annotation)
    | USub, Constant_f(f, _) -> Constant_f(-.f, annotation)
    | Float_of_int, Constant_i(i, _) -> Constant_f(float_of_int i, annotation)
    | Floor, Constant_f(f, _) -> Constant_i(int_of_float (floor f), annotation)
    | Cos , Constant_f(f, _) -> Constant_f(cos f, annotation)
    | Sin , Constant_f(f, _) -> Constant_f(sin f, annotation)
    | _ -> Unary_operator(op, expr', annotation)
    )

  | List (expr_list, annotation) -> List (List.map simplifier_expr expr_list, annotation)
  | Cons(expr1, expr2, annotation) -> Cons(simplifier_expr expr1, simplifier_expr expr2, annotation)
  | Field_accessor (field, expr, annotation) -> Field_accessor (field, simplifier_expr expr, annotation)
  | Ternary_operator (expr1, expr2, expr3, annotation) ->(let expr1' = simplifier_expr expr1 in
    match expr1' with
    | Constant_b(true, _) -> simplifier_expr expr2
    | Constant_b(false, _) -> simplifier_expr expr3
    | _ -> Ternary_operator(expr1', simplifier_expr expr2, simplifier_expr expr3, annotation)
    )


let rec simplify_statement statement =
  match statement with
  | Assignment(expr,expr2,annotation) -> Assignment(simplifier_expr expr, simplifier_expr expr2, annotation)
  | Variable_declaration(name,type_expr,annotation) -> Variable_declaration(name,type_expr,annotation)
  | Block(statements,annotation) -> Block(List.map simplify_statement statements, annotation)
  | IfThenElse(expr,statement1,statement2,annotation) -> (
    let expr' = simplifier_expr expr in
    match expr' with
    | Constant_b(true, _) -> simplify_statement statement1
    | Constant_b(false, _) -> simplify_statement statement2
    | _ -> IfThenElse(expr', simplify_statement statement1, simplify_statement statement2, annotation)
    )
  | For(name,expr1,expr2,expr3,statement,annotation) -> For(name, simplifier_expr expr1, simplifier_expr expr2, simplifier_expr expr3, simplify_statement statement, annotation)
  | Foreach(name,expr,statement,annotation) -> Foreach(name, simplifier_expr expr, simplify_statement statement, annotation)
  | Draw(expr,annotation) -> Draw(simplifier_expr expr, annotation)
  | Nop -> Nop
  | Print(expr,annotation) -> Print(simplifier_expr expr, annotation)
  | While(expr,statement,annotation) ->(let expr',statement' = simplifier_expr expr, simplify_statement statement in
    match expr' with
    | Constant_b(true, _) -> While(expr', statement', annotation)
    | Constant_b(false, _) -> Nop
    | _ -> While(expr', statement', annotation)
    )
  | _ -> statement





let simplifier program =
  match program with
  | Program(arg_list, statement) -> Program(arg_list, simplify_statement statement)
