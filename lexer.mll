{
  open Parser
  exception SyntaxError of string

  let reserved = [
      "annotation"; "any"; "case"; "else"; "elseif"; "endif"; "enum"; "function";
      "if"; "include"; "let"; "output"; "par"; "record"; "string"; "test";
      "then"; "tuple"; "type"; "where" ]
  let kwds = [
      "array", ARRAY; "set", SET; "of", OF; "var", VAR;
      "bool", BOOL; "false", BOOL_LIT false; "true", BOOL_LIT true;
      "constraint", CONSTRAINT; "predicate", PREDICATE; "solve", SOLVE;
      "float", FLOAT; "int", INT;
       "satisfy", SATISFY; "maximize", MAXIMIZE; "minimize", MINIMIZE ]
  let keyword_or_ident =
    let keywords = Hashtbl.create 37 in
    let () = List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok) kwds in
    fun s -> match Hashtbl.find_opt keywords s with
             | None -> if List.mem s reserved then failwith ("reserved: " ^ s)
                       else IDENTIFIER s
             | Some token -> token
}

let digit = ['0'-'9']
let integer = ['-''+']? digit+

rule token = parse
| '%' [^'\n']* '\n' { Lexing.new_line lexbuf; token lexbuf }
| [' ''\t'] { token lexbuf }
| ['\r''\n'] { Lexing.new_line lexbuf; token lexbuf }
| eof { token lexbuf }
| ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as s { keyword_or_ident s }
| '_'*['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_']* as s { U_IDENTIFIER s }
| ',' { COMMA }
| ':' { COLON }
| ';' { SEMICOLON }
| '=' { EQ }
| ".." { DOUBLEDOT }
| "::" { DOUBLECOLON }
| '(' { LP }
| ')' { RP }
| '{' { LCB }
| '}' { RCB }
| '[' { LSB }
| ']' { RSB }
| integer as i { INT_LIT (int_of_string i) }
| '-'? "0x" ['0'-'9''A'-'F''a'-'f']+ as i { INT_LIT (int_of_string i) }
| '-'? "0o" ['0'-'7']+ as i { INT_LIT (int_of_string i) }
| integer '.' digit+ (['E''e'] integer)? as f { FLOAT_LIT (float_of_string f) }
| integer ['E''e'] integer as f { FLOAT_LIT (float_of_string f) }
| '"' [^'"''\n']* '"' as s { STRING_LIT s }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
