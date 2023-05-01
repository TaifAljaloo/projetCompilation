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
    Par exemple : type_expression : Ast.type_expr Util.Environment.t -> Util.Error_report.t -> Ast.expression -> Ast.type_expr
    (cette fonction peut également ne rien renvoyer, auquel cas vous devrez avoir une fonction auxiliaire récupérant le type d’une expression déjà typée).

    Vous pouvez également effectuer ici (en même temps ou dans une fonction séparée) une analyse d’initialisation des variables (auquel cas, il faut ajouter un argument supplémentaire à ce qui est décrit ci-dessus).

    Vous préciserez ce que vous avez traité dans votre rapport.
*)
let string_of_expr expr =
  match expr with 
  | Some t -> Ast.string_of_type_expr t
  | _ -> "None"
  let argument_analyser environment argument = 
    match argument with
    | Argument(name, var_type, _) -> Environment.add environment name var_type
  
  let argument_list_analyser environment arg_list = 
    List.iter (argument_analyser environment) arg_list
  let rec type_expression type_environment report expression =
    match expression with
    | Constant_i (_,annotation) -> Annotation.get_type(Annotation.set_type annotation Type_int)
    | Constant_b (_,annotation) -> Annotation.get_type(Annotation.set_type annotation Type_bool)
    | Constant_f (_,annotation) -> Annotation.get_type(Annotation.set_type annotation Type_float)
    | Pos(x,y,annotation) -> (let tx,ty = type_expression type_environment report x, type_expression type_environment report y in
      (match tx with 
      | Some Type_int -> (match ty with
                        | Some Type_int -> Annotation.get_type(Annotation.set_type annotation Type_pos)
                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in pos between %s and %s" (string_of_expr tx) (string_of_expr ty),(Annotation.get_pos annotation));None)
      | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in pos between %s and %s" (string_of_expr tx) (string_of_expr ty),(Annotation.get_pos annotation));None))
    | Color(r,g,b,annotation) -> (
      let tr,tg,tb = type_expression type_environment report r, type_expression type_environment report g, type_expression type_environment report b in
      (match tr with 
      | Some Type_int -> (match tg with
                        | Some Type_int -> (match tb with
                                            | Some Type_int -> Annotation.get_type(Annotation.set_type annotation Type_color)
                                            | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in color between %s and %s" (string_of_expr tr) (string_of_expr tb),(Annotation.get_pos annotation));None)
                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in color between %s and %s" (string_of_expr tr) (string_of_expr tg),(Annotation.get_pos annotation));None)
      | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in color between %s and %s" (string_of_expr tr) (string_of_expr tg),(Annotation.get_pos annotation));None))
    | Point(pos,c,anno) -> (
      let tpos,tc = type_expression type_environment report pos, type_expression type_environment report c in
      (match tpos with 
      | Some Type_pos -> (match tc with
                        | Some Type_color -> Annotation.get_type(Annotation.set_type anno Type_point)
                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in point between %s and %s" (string_of_expr tpos) (string_of_expr tc),(Annotation.get_pos anno));None)
      | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in point between %s and %s" (string_of_expr tpos) (string_of_expr tc),(Annotation.get_pos anno));None))   
    | Variable(name,annotation) -> (match Environment.get type_environment name with
                                    | Some t -> Annotation.get_type(Annotation.set_type annotation t)
                                    | None -> Error_report.add_error report (Format.sprintf "Error: variable %s not declared" name,(Annotation.get_pos annotation));None)
    | Binary_operator(op,expr1,expr2,anno) -> (let texpr1,texpr2 = type_expression type_environment report expr1, type_expression type_environment report expr2 in
      match texpr1,texpr2 with
      | Some Type_int, Some Type_int -> (match op with
                                        | Add | Sub | Mul | Div | Mod | Pow -> Annotation.get_type(Annotation.set_type anno Type_int)
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_float, Some Type_float -> (match op with
                                        | Add | Sub | Mul | Div | Mod |Pow  -> Annotation.get_type(Annotation.set_type anno Type_float)
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_bool, Some Type_bool -> (match op with
                                        | And | Or -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_pos, Some Type_pos -> (match op with
                                        | Add | Sub | Mul | Div | Mod -> Annotation.get_type(Annotation.set_type anno Type_pos)
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_color, Some Type_color -> (match op with
                                        | Add | Sub | Mul | Div | Mod-> Annotation.get_type(Annotation.set_type anno Type_color)
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_list(t), Some Type_list(t') -> (match op with
                                        | Add when t = t' -> Annotation.get_type(Annotation.set_type anno (Type_list(t)))
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_int ,Some Type_float -> (match op with
                                        | Add | Sub | Mul | Div | Mod |Pow -> Annotation.get_type(Annotation.set_type anno Type_float)
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_float, Some Type_int -> (match op with
                                        | Add | Sub | Mul | Div | Mod | Pow -> Annotation.get_type(Annotation.set_type anno Type_float)
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | Some Type_point, Some Type_point -> (match op with
                                        | Add | Sub | Mul | Div | Mod -> Annotation.get_type(Annotation.set_type anno Type_point)
                                        | Eq| Ne | Lt | Le | Gt | Ge -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)
      | _,_-> Error_report.add_error report (Format.sprintf "Error: type mismatch in binary operator between %s and %s" (string_of_expr texpr1) (string_of_expr texpr2),(Annotation.get_pos anno));None)

  | Unary_operator(op, expr, anno) -> ( let expr_type = type_expression type_environment report expr in 
  match expr_type with
    | Some Type_int -> (match op with
                                        | USub -> Annotation.get_type(Annotation.set_type anno Type_int)
                                        | Float_of_int -> Annotation.get_type(Annotation.set_type anno Type_float)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in unary operator between %s and %s" (string_of_expr expr_type) (string_of_expr expr_type),(Annotation.get_pos anno));None)
    | Some Type_float -> (match op with
                                        | USub -> Annotation.get_type(Annotation.set_type anno Type_float)
                                        | Floor -> Annotation.get_type(Annotation.set_type anno Type_int)
                                        | Cos | Sin -> Annotation.get_type(Annotation.set_type anno Type_float)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in unary operator between %s and %s" (string_of_expr expr_type) (string_of_expr expr_type),(Annotation.get_pos anno));None)
    | Some Type_bool -> (match op with
                                        | Not -> Annotation.get_type(Annotation.set_type anno Type_bool)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in unary operator between %s and %s" (string_of_expr expr_type) (string_of_expr expr_type),(Annotation.get_pos anno));None)  
    | Some Type_list(t) -> (match op with 
                                        | Head -> Annotation.get_type(Annotation.set_type anno t)
                                        | Tail -> Annotation.get_type(Annotation.set_type anno (Type_list(t)))
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in unary operator between %s and %s" (string_of_expr expr_type) (string_of_expr expr_type),(Annotation.get_pos anno));None)
    | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in unary operator between %s and %s" (string_of_expr expr_type) (string_of_expr expr_type),(Annotation.get_pos anno));None)
  | Field_accessor(accessor,var,anno) -> (let var_type = type_expression type_environment report var in 
                            match var_type with
    | Some Type_point -> (match accessor with
                                        | Color_accessor -> Annotation.get_type(Annotation.set_type anno Type_color)
                                        | Position_accessor -> Annotation.get_type(Annotation.set_type anno Type_pos)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in field accessor between %s and %s" (string_of_expr var_type) (string_of_expr var_type),(Annotation.get_pos anno));None)
    | Some Type_color -> (match accessor with
                                        | Red_accessor | Green_accessor | Blue_accessor -> Annotation.get_type(Annotation.set_type anno Type_int)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in field accessor between %s and %s" (string_of_expr var_type) (string_of_expr var_type),(Annotation.get_pos anno));None)
    | Some Type_pos -> (match accessor with
                                        | X_accessor | Y_accessor -> Annotation.get_type(Annotation.set_type anno Type_int)
                                        | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in field accessor between %s and %s" (string_of_expr var_type) (string_of_expr var_type),(Annotation.get_pos anno));None)
    | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in field accessor between %s and %s" (string_of_expr var_type) (string_of_expr var_type),(Annotation.get_pos anno));None)
  |List(l,a)->(match Annotation.get_type a with
    | Some t -> Some t
    | _-> (match l with
          | [] -> None
          | [e]->let et=type_expression  type_environment report e in
                  (match et with
                  | Some t -> (Annotation.get_type(Annotation.set_type a (Type_list(t))))
                  |_->Error_report.add_error report (Format.sprintf "Error: cant infer type of list",(Annotation.get_pos a));None)
          | e::l->(let et,lt=type_expression  type_environment report e,type_expression type_environment report (List(l,a)) in
                  match et,lt with
                  | _,None -> None
                  | Some te,Some Type_list(tl) when(te=tl)->Some (Type_list(tl))
                  | Some _,Some Type_list(_) -> Error_report.add_error report (Format.sprintf "Error: type mismatch in list between %s and %s" (string_of_expr et) (string_of_expr lt),(Annotation.get_pos a));None
                  | _,_ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in list between %s and %s" (string_of_expr et) (string_of_expr lt),(Annotation.get_pos a));None
                  )))
|Cons(e,list,a)->(let et,lt=type_expression type_environment report e,type_expression type_environment report list in
      match et,lt,list with
      | Some te,_,List([],_) -> Annotation.get_type(Annotation.set_type a (Type_list(te)))
      | Some te,Some tl,_ -> (match tl with
                            | Type_list(t) when(te=t)->Annotation.get_type(Annotation.set_type a (Type_list(t)))
                            | Type_list(_) -> Error_report.add_error report (Format.sprintf "Error: type mismatch in list between %s and %s" (string_of_expr et) (string_of_expr lt),(Annotation.get_pos a));None
                            | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in list between %s and %s" (string_of_expr et) (string_of_expr lt),(Annotation.get_pos a));None)
      |Some te,_,_ -> (match list with
                      | List(l,_)->(match l with
                                    | [] -> Annotation.get_type(Annotation.set_type a (Type_list(te)))
                                    | _::l -> (let lt=type_expression type_environment report (List(l,a)) in
                                              match lt with
                                              | Some Type_list(tl) when(te=tl)->Annotation.get_type(Annotation.set_type a (Type_list(te)))
                                              | Some Type_list(_) -> Error_report.add_error report (Format.sprintf "Error: type mismatch in list between %s and %s" (string_of_expr et) (string_of_expr lt),(Annotation.get_pos a));None
                                              | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in list between %s and %s" (string_of_expr et) (string_of_expr lt),(Annotation.get_pos a));None))

      | _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in list between %s and %s" (string_of_expr et) (string_of_expr lt),(Annotation.get_pos a));None)
      | None,_,_ -> None)

  

let rec type_instruction type_environment report instruction=
    match instruction with
  | Variable_declaration(name, var_type, _) -> (Environment.add type_environment name var_type)
  | Block(s,annotation) -> ( match s with
                            | [] -> Error_report.add_warning report (Format.sprintf "Warning: empty block",(Annotation.get_pos annotation));()
                            | _ -> Environment.add_layer type_environment; List.iter (fun i -> type_instruction type_environment report i) s; Environment.remove_layer type_environment)
  |IfThenElse(e,_,_,annotation) -> ( let et=type_expression type_environment report e in
                                    match et with
                                    | Some Type_bool -> ()
                                    | Some _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in if condition between %s and %s" (string_of_expr et) (Ast.string_of_type_expr Type_bool),(Annotation.get_pos annotation));()
                                    | _-> ())
  | For(_,start,finish,step,body,anno) -> (let start_type, finish_type, step_type = type_expression type_environment report start, type_expression type_environment report finish, type_expression type_environment report step in
                                    (match start_type with
                                    | Some Type_int | Some Type_float -> ()
                                    |  _ ->Error_report.add_error report( Format.sprintf "%s is not a valid iterator type."
                                            (string_of_expr start_type),(Annotation.get_pos anno)));
                                    if finish_type <> start_type then
                                      Error_report.add_error report (Format.sprintf "Error: type mismatch in for loop between %s and %s" (string_of_expr start_type) (string_of_expr finish_type),(Annotation.get_pos anno));
                                    if step_type <> start_type then
                                      Error_report.add_error report (Format.sprintf "Error: type mismatch in for loop between %s and %s" (string_of_expr start_type) (string_of_expr step_type),(Annotation.get_pos anno));
                                    type_instruction type_environment report body)
                                      
                            
  | Foreach(var,expr,body,annotation) -> (let et,vt = type_expression type_environment report expr,Environment.get type_environment var  in
                                          match et,vt with
                                          | Some Type_list(t),Some Type_list(t2) -> (match t,t2 with
                                                              | Type_point,Type_point -> (type_instruction type_environment report body;())
                                                              | Type_color,Type_color -> (type_instruction type_environment report body;())
                                                              | Type_int,Type_int -> (type_instruction type_environment report body;())
                                                              | _,_ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in foreach between %s and %s" (string_of_expr et) (string_of_expr vt),(Annotation.get_pos annotation));())
                                          | Some Type_list(t),Some t2 -> (match t,t2 with
                                                              | Type_point,Type_point -> (type_instruction type_environment report body;())
                                                              | Type_color,Type_color -> (type_instruction type_environment report body;())
                                                              | Type_int,Type_int -> (type_instruction type_environment report body;())
                                                              | _,_ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in foreach between %s and %s" (string_of_expr et) (string_of_expr vt),(Annotation.get_pos annotation));())
                                          | Some t,Some Type_list(t2) -> (match t,t2 with
                                                              | Type_point,Type_point -> (type_instruction type_environment report body;())
                                                              | Type_color,Type_color -> (type_instruction type_environment report body;())
                                                              | Type_int,Type_int -> (type_instruction type_environment report body;())
                                                              | _,_ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in foreach between %s and %s" (string_of_expr et) (string_of_expr vt),(Annotation.get_pos annotation));())
                                          | Some t,Some t2 -> (match t,t2 with
                                                              | Type_point,Type_point -> (type_instruction type_environment report body;())
                                                              | Type_color,Type_color -> (type_instruction type_environment report body;())
                                                              | Type_int,Type_int -> (type_instruction type_environment report body;())
                                                              | _,_ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in foreach between %s and %s" (string_of_expr et) (string_of_expr vt),(Annotation.get_pos annotation));())
                                          | _,_ -> ())
  | Nop -> ()
  | Draw(e,annotation) -> (let et=type_expression type_environment report e in
                        match et with
                        | Some Type_point -> ()
                        | Some _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in draw between %s and %s" (string_of_expr et) (Ast.string_of_type_expr Type_point),(Annotation.get_pos annotation));()
                        | _ -> ())
  | Print(_,_) -> ()
  | Assignment(var_name, value, assi_anno) -> (match var_name with
    | Variable(name, anno) -> (let variable_type = Environment.get type_environment name in
    match variable_type, value with
    | Some Type_list(_), List([], _) -> ()
    | Some t,_ -> (let value_type = type_expression  type_environment report value in
      match value_type with
      | Some vt when (t=vt) -> ()
      | Some vt -> Error_report.add_error report (Format.sprintf "Type %s is invalide for %s variable" (Ast.string_of_type_expr vt) name, Annotation.get_pos anno)
      | _ -> Error_report.add_error report ("Unable to determine type", Annotation.get_pos anno))
    | _ -> Error_report.add_error report (Format.sprintf "Variable %s undefined" name, Annotation.get_pos anno))
    | Field_accessor(field,_,anno) -> (let field_type, value_type = type_expression type_environment report  var_name, type_expression  type_environment report value in 
    match field_type, value_type with
    | Some ft, Some vt when (ft=vt) -> ()
    | Some _, Some vt -> Error_report.add_error report (Format.sprintf "Type %s is invalide for %s fiel accessor" (Ast.string_of_type_expr vt) (Ast.string_of_field_accessor field), Annotation.get_pos anno)
    | _ -> Error_report.add_error report ("Unable to determine type", Annotation.get_pos anno))
    
  | _ -> Error_report.add_error report ("Error in Assigment first argument is not a variable", Annotation.get_pos assi_anno))
  | While(expr,stat,annotation) -> (let et = type_expression type_environment report expr in
                                    match et with
                                    | Some Type_bool -> type_instruction type_environment report stat
                                    | Some _ -> Error_report.add_error report (Format.sprintf "Error: type mismatch in while between %s and %s" (string_of_expr et) (Ast.string_of_type_expr Type_bool),(Annotation.get_pos annotation));()
                                    | _ -> ())
  |Init_Variable_declaration(var_name, var_type,var_value, annotation) -> (let variable_value = type_expression type_environment report var_value in
                                                                          match variable_value with
                                                                          | Some vt when (var_type=vt) -> Environment.add type_environment var_name var_type
                                                                          | Some vt -> Error_report.add_error report (Format.sprintf "Type %s is invalide for %s variable" (Ast.string_of_type_expr vt) var_name, Annotation.get_pos annotation)
                                                                          | _ -> Error_report.add_error report ("Unable to determine type", Annotation.get_pos annotation))



  let type_analyser report program =
    let type_environment = Environment.new_environment () in
    match program with Program(l,s) ->  argument_list_analyser type_environment l; type_instruction type_environment report s;()
  