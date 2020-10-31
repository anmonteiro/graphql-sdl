module Ast = Ast
module Printer = Graphql_schema_printer

let schemaDescription =
  {|input TodoInput {
  title: String!
    active: Boolean!
 }
 "TODO DESCRIPTION"
 type Todo {
   "description on field"
  id: ID!
    title: String!
    active: Boolean!
 }
  type Query {
    todos: [Todo]
  }
 type Mutation {
  deleteTodo("arg descr" id: ID!, other: Boolean!): [Todo]
  toggleTodo(id: ID!): [Todo]
  addTodo(title: String!, active: Boolean!): [Todo]
 }

   enum Episode {
  NEWHOPE
  EMPIRE
  JEDI
}

"Description"
type Foo implements Some_Intf {
  bar(
    "arg description"
    arg: Int
  ): Int!
}
   scalar Url

   union SearchResult = Human | Droid | Starship

   type Foo {
     someCall(first: Int = 5): [Todo]
     otherCall(inputObject: TodoInput = {title: "do something"}): [Todo]
   }

   schema @amigo {
     query: Query
     mutation: Mutation
   }
   |}

let print_position lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Graphql_schema_parser.doc Graphql_schema_lexer.token lexbuf with
  | Graphql_schema_parser.Error ->
    Printf.eprintf "%s: syntax error\n" (print_position lexbuf);
    exit (-1)

let parse_and_print lexbuf = parse_with_error lexbuf

let parse schema =
  let lexbuf = Lexing.from_string schema in
  parse_and_print lexbuf

let read_all path =
  let file = open_in path in
  try really_input_string file (in_channel_length file) with
  | exn ->
    close_in file;
    raise exn

let print () =
  Easy_format.Pretty.to_stderr
    (Graphql_schema_printer.pprint_schema (parse schemaDescription))
