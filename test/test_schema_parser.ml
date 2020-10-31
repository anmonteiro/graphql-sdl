let () =
  Alcotest.run
    "GraphQL SDL parser unit tests"
    [ "GraphQL SDL tests", Schema_parser_test.tests ]
