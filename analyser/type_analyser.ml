[@@@ocaml.warning "-8"]
open Util
open Ast

(*
    Réalisez ici l’analyse de type d’un programme. Faites une sous-fonction récursive pour les expressions et les statements.

    L’idée est la même que dans le langage du cours : pour chaque élément, commencez par typer ses sous-éléments. Ensuite, en fonction de l’élément et du type de ses sous-éléments, déterminez son type, et placez-le dans l’annotation qui l’accompagne.
    Seules les expressions peuvent ainsi être typées.

    Les fonctions devront manipuler un environnement qui associera chaque variable à son type. Les déclarations fixent ces types et ces types sont vérifiés lors de chaque utilisation d’une variable.

    Attention, les déclarations sont locales à un bloc, vous devez donc utiliser les fonctions Environment.add_layer et Environment.remove_layer pour le gérer.

    Cette fonction doit également effectuer les rapports d’erreur et d’avertissement dans [report].

    Ces fonction font un pattern matching sur leur argument principal et traitent chaque cas séparément. Elles font uniquement des effet de bord.
    Par exemple : type_expression : Ast.type_expr annotation -> Util.Error_report.t -> Ast.expression -> unit

    Vous pouvez également effectuer ici (en même temps ou dans une fonction séparée) une analyse d’initialisation des variables (auquel cas, il faut ajouter un argument supplémentaire à ce qui est décrit ci-dessus).

    Vous préciserez ce que vous avez traité dans votre rapport.
*)
let type_to_string t =
  match t with Some t -> string_of_type_expr t | None -> "Null"
let rec type_expr expr init_env t_env report =
  match expr with
  | Constant_i (_,a) -> Annotation.set_type a Type_int
  | Constant_b (_,a) -> Annotation.set_type a Type_bool
  | Constant_f (_,a) -> Annotation.set_type a Type_float
  | Pos(_,_,a) -> Annotation.set_type a Type_pos
  | Color(_,_,_,a) -> Annotation.set_type a Type_color
  | Point(_,_,a) -> Annotation.set_type a Type_point
  | Variable(name,a) -> (
    match Environment.get t_env name with
    | Some t ->(
      match Environment.get init_env name with
      | Some true -> Annotation.set_type a t
      | _-> Error_report.add_warning report (Format.sprintf "Variable %s is not initialized" name, Annotation.get_pos a); a
    )
    | None -> Error_report.add_error report (Format.sprintf "Variable %s is not declared" name, Annotation.get_pos a); a
  )
  | Binary_operator(op,expr1,expr2,a)-> (
    let t1 = Annotation.get_type (type_expr expr1 init_env t_env report)
    and t2 = Annotation.get_type (type_expr expr2 init_env t_env report) in
    match op with
    | And | Or | Eq | Ne | Lt | Le | Gt | Ge -> Annotation.set_type a Type_bool
    | _ -> if t1 <> t2 then (Error_report.add_error report (Format.sprintf "Type mismatch between %s and %s" (type_to_string t1) (type_to_string t2), Annotation.get_pos a); a)
    else  
      match Annotation.get_type (type_expr expr1 init_env t_env report) with
      | Some t -> Annotation.set_type a t
      | None -> a)
  | Unary_operator(op,expr,a) -> (
    let t = Annotation.get_type (type_expr expr init_env t_env report) in
    match op with
    | Floor -> Annotation.set_type a Type_int
    | Float_of_int | Cos | Sin-> Annotation.set_type a Type_float
    | Not -> Annotation.set_type a Type_bool
    | USub | Head | Tail -> (match t with Some t -> Annotation.set_type a t | None -> a))
  | Field_accessor(f,_,a) -> (
    match f with
    | Color_accessor -> Annotation.set_type a Type_color
    | Position_accessor -> Annotation.set_type a Type_pos
    | X_accessor | Y_accessor -> Annotation.set_type a Type_int
    | Blue_accessor | Green_accessor | Red_accessor -> Annotation.set_type a Type_int)
  | List(l,a) -> (
    let t=List.map ( fun e -> type_expr e init_env t_env report) l in
    match Annotation.get_type (List.hd t) with
    | Some t -> Annotation.set_type a (Type_list t)
    | None -> a)
  | Cons(expr1,expr2,a) -> (
    let t1 = Annotation.get_type (type_expr expr1 init_env t_env report)
    and t2 = Annotation.get_type (type_expr expr2 init_env t_env report) in
    if t1 <> t2 then (Error_report.add_error report (Format.sprintf "Type mismatch between %s and %s" (type_to_string t1) (type_to_string t2), Annotation.get_pos a); a)
    else
      match t1 with
      | Some t -> Annotation.set_type a (Type_list t)
      | None -> a)

let rec type_stat stat init_env t_env report =
  match stat with 
  | Assignment(expr1,expr2,a)-> Environment.add init_env(Ast.string_of_expression expr1) true;
  let t1 = Annotation.get_type (type_expr expr1 init_env t_env report) and 
  t2 = Annotation.get_type (type_expr expr2 init_env t_env report) in
  if t1 <> t2 then (Error_report.add_error report (Format.sprintf "Type mismatch between %s and %s" (type_to_string t1) (type_to_string t2), Annotation.get_pos a)
  )
  | Variable_declaration(name,expr,a) -> Environment.add t_env name expr
  | Block (l,a) -> List.iter (fun s -> type_stat s init_env t_env report) l
  | IfThenElse(expr,stat1,stat2,a) ->
    let t = Annotation.get_type (type_expr expr init_env t_env report) in
    if t <> Some Type_bool then
      Error_report.add_error report
        ( Format.sprintf "\"%s\" should be of type Bool but is of type %s."
            (string_of_expression expr)
            (type_to_string t),
          Annotation.get_pos a );
    type_stat stat1 init_env t_env report;
    type_stat stat2 init_env t_env report
  | Print(expr,_) -> ignore (type_expr expr init_env t_env report)
  | _ -> ()


let type_analyser report program =
  let type_environment = Environment.new_environment () 
  and init_environment = Environment.new_environment () in
  match program with 
  | Program(_,s) -> type_stat s init_environment type_environment report

    