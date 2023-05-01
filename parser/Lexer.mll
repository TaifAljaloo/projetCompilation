
{
    open Parser
    exception Error of string
}

let digit = ['0' - '9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
    | "//" [^ '\n']* '\n' {Lexing.new_line lexbuf; token lexbuf}
    | "/*"              {commentary lexbuf}
    | [' ' '\t' '\r']   {token lexbuf}
    | '\n'              { Lexing.new_line lexbuf ; token lexbuf }
    | "Int"             { TYPE_INT }
    | "Float"           { TYPE_FLOAT }
    | "Bool"            { TYPE_BOOL }
    | "Pos"             { POS }
    | "Point"           { TYPE_POINT }
    | "List"            { TYPE_LIST }
    | "Color"           { COLOR }
    | "+"               { ADD }
    | "-"               { SUB }
    | "*"               { MUL }
    | "/"               { DIV }
    | "%"               { MOD }
    | "::"              { CONCAT }
    | "And"             { AND }
    | "Or"              { OR }
    | "="               { EQ }
    | "<>"              { NE }
    | "<"               { LT }
    | ">"               { GT }
    | "<="              { LE }
    | ">="              { GE }
    | "Not"             { NOT }
    | "Head"            { HEAD }
    | "Tail"            { TAIL }
    | "Floor"           { FLOOR }
    | "Float_of_int"    { FLOAT_OF_INT }
    | "Cos"             { COS }
    | "Sin"             { SIN }
    | ";"               { SEMICOLON }
    | ","               { COMMA }
    | "("               { L_PAR }
    | ")"               { R_PAR }
    | "["               { L_SQ_BRK }
    | "]"               { R_SQ_BRK }
    | "Begin"           { BEGIN }
    | "End"             { END }
    | "Nop"             { NOP }
    | "Draw"            { DRAW }
    | "Foreach"         { FOREACH }
    | "In"              { IN }
    | "Copy"            { COPY }
    | "For"             { FOR }
    | "From"            { FROM }
    | "To"              { TO }
    | "Step"            { STEP }
    | "Pi"              { PI }
    | "Print"           { PRINT }
    | "Cons"            { CONS }
    | "True"            { TRUE }
    | "False"           { FALSE }
    | "If"              { IF }
    | "Else"            { ELSE }
    | "X"               { X_ACCESSOR }
    | "Y"               { Y_ACCESSOR }
    | "Red"             { RED_ACCESSOR }
    | "Green"           { GREEN_ACCESSOR }
    | "Blue"            { BLUE_ACCESSOR }
    | "."               { DOT }
    | "While"           { WHILE }
    | "?"               {TERNARY}
    | ":"              {COLON}  
    | (digit)* "." (digit)* as s {FLOAT(try float_of_string s with Failure _ -> raise (Error(s)) )}
    | (digit)+ as s     { INT(try int_of_string s with Failure _ ->(let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%s'. It is not a valid integer" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) ))}
    | eof               { EOF }
    | ['a'-'z' 'A'-'Z'] (alphanum)* as s  { ID(s) }
    | _ as s            { let pos = Lexing.lexeme_start_p lexbuf in raise (Error(Format.sprintf "Line %d, char %d ,Read: '%c'. It is not an acceptable character" pos.pos_lnum (pos.pos_cnum - pos.pos_bol +1) s)) }

and commentary = parse
    | '\n'      {Lexing.new_line lexbuf; commentary lexbuf}
    | "*/"      { token lexbuf }
    | _ { commentary lexbuf }