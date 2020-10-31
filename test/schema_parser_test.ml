let graphql_sdl =
  (module struct
    type t = Graphql_sdl.Ast.optype list

    let pp formatter t =
      let open Graphql_sdl in
      t |> Printer.pprint_schema |> Easy_format.Pretty.to_formatter formatter

    let equal = ( = )
  end : Alcotest.TESTABLE
    with type t = Graphql_sdl.Ast.optype list)

let pprint_ast ast =
  let open Graphql_sdl in
  ast |> Printer.pprint_schema |> Easy_format.Pretty.to_string

let test_parser_printer_bidirectionality sdl =
  let ast = Graphql_sdl.parse sdl in
  let printed = pprint_ast ast in
  Alcotest.check
    graphql_sdl
    "Test parsing printed AST is equal to the parsed AST"
    ast
    (Graphql_sdl.parse printed)

let tests =
  [ ( "one object, one attribute"
    , `Quick
    , fun () -> test_parser_printer_bidirectionality "type Foo { bar: Int! }" )
  ; ( "input type and lists too"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo { bar: Int! } input Bar { bar: [Int]! }" )
  ; ( "type descriptions"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "\n\
          \      \"Description\" type Foo { bar: Int! }\n\
          \      \"Description\" input Bar { bar: [Int]! }\n\
          \      \"Description\" enum Baz{ \"On Enum Value\" QUX }\n\
          \      \"Description\" union Baz = | Foo\n\
          \      \"Description\" scalar Wow\n\
          \      " )
  ; ( "multiline type description"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          {|
      """Multiline
      Description"""
      type Foo { bar: Int! } input Bar { bar: [Int]! }
      |}
    )
  ; ( "descriptions in argument definitions"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "\n\
          \      \"Description\" type Foo { bar(\"arg description\" arg: Int): \
           Int! }\n\
          \      " )
  ; ( "recursive type"
    , `Quick
    , fun () -> test_parser_printer_bidirectionality "type Foo { bar: Foo! }" )
  ; ( "arguments"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo { bar(baz: Int!): Foo! }\n\
          \       type Bar { baz(more: Int!, than: String, one: String!): Foo! \
           }" )
  ; ( "type Query"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo { bar(baz: Int!): Foo! } type Query { foo: Foo }" )
  ; ( "type Query"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo { bar(baz: Int!): Foo! } type Query { foo: Foo }" )
  ; ( "schema and mutations"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo { bar(baz: Int!): Foo! }\n\
           type RootQueryType { foo: Foo }\n\
           type MutationType { foobar: Foo }\n\
           schema {\n\
          \  query: RootQueryType\n\
          \  mutation: MutationType\n\
           }" )
  ; ( "directives on schema"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality "schema @hello { query: Foo }" )
  ; ( "directives on types"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo @hello(world: \"hello\"){ bar: Foo! }\n\
          \       input Foo @hello(world: \"hello\"){ bar: Foo! }" )
  ; ( "directives on interfaces"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "interface Foo @hello(world: \"hello\"){ bar: Foo! }" )
  ; ( "multiple directives on types"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo @hello(world: \"hello\") @other(directive: 42){ bar: Foo! }\n\
          \       input Foo @hello(world: \"hello\") @other(directive: 42) { \
           bar: Foo! }" )
  ; ( "directives on enums, unions and scalars"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "enum Foo @hello(world: \"hello\") { BAR BAZ }\n\
          \       union Bar @hello(world: \"hello\") = | Foo\n\
          \       scalar Something @hello(world: \"hello\")" )
  ; ( "directives on fields"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "type Foo { bar: Foo! @hello }\n       input Foo { bar: Foo! @hello }"
    )
  ; ( "interfaces"
    , `Quick
    , fun () ->
        test_parser_printer_bidirectionality
          "interface Node { id: ID!  }\n\
          \       type Foo implements Node { id: ID! name: String! }" )
  ]
