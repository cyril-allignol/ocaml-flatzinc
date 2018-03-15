let model = fun file ->
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ch in
  try
    let model = Parser.model Lexer.token lexbuf in
    close_in ch;
    model
  with e ->
    let open Lexing in
    Printf.eprintf
      "\nSyntax error line %d, characters %d-%d (token: %s)\n%!"
      lexbuf.lex_start_p.pos_lnum
      (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
      (Lexing.lexeme lexbuf);
    raise e
