(* Copyright 2018 Cyril Allignol
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. *)

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
