%{
open Ast
open Graphql_parser

let rec has_duplicates = function
  | [] -> false
  | x::xs ->
    if (List.filter (fun y -> y = x) xs == []) then
      has_duplicates xs
    else
      true
%}

%token <string> NAME
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token COLON
%token EQUAL
%token BANG
%token PIPE
%token AT
%token NULL
%token ON
%token QUERY
%token MUTATION
%token SUBSCRIPTION
%token TYPE
%token INPUT
%token ENUM
%token UNION
%token INTERFACE
%token IMPLEMENTS
%token SCALAR
%token SCHEMA

%token EOF

%start doc
%type <Ast.optype list> doc

%%

%inline default_list(X):
  | X { $1 }
  | { [] }

%inline lseparated_list(sep, X):
  | (* empty *) { [] }
  | lseparated_nonempty_list(sep, X) { $1 };

%inline lseparated_nonempty_list(sep, X):
  lseparated_nonempty_list_aux(sep, X) { List.rev $1 };

lseparated_nonempty_list_aux(sep, X):
  | X { [$1] }
  | lseparated_nonempty_list_aux(sep, X) sep X { $3 :: $1 }

doc:
  | definition+ EOF { $1 }

definition:
  | type_definition
  | input_type_definition
  | enum_definition
  | union_definition
  | interface_definition
  | scalar_definition
  | schema_definition { $1 }

%inline optional_description:
  | (* empty *) { None }
  | STRING { Some $1 }
  (* | MULTILINE_STRING *)

type_definition:
  | optional_description TYPE name optional_implementations directives field_set
    {
      Type {
        name = $3;
        field_defs = $6;
        directives = $5;
        description = $1;
        interfaces = Some $4;
      }
    }

optional_implementations:
  | (* empty *) { [] }
  | optional_implementations IMPLEMENTS name { $3 :: $1 }

input_type_definition:
  | optional_description INPUT name directives field_set
    {
      InputType {
        name = $3;
        field_defs = $5;
        directives = $4;
        description = $1;
        interfaces = None
      }
    }

interface_definition:
  | optional_description INTERFACE name directives field_set
    {
      Interface {
        name = $3;
        field_defs = $5;
        directives = $4;
        description = $1;
        interfaces = None
      }
    }

enum_definition:
  | optional_description ENUM name directives enum_vals
    {
      Enum {
        name = $3;
        possibleVals = $5;
        directives = $4;
        description = $1;
      }
    }

union_definition:
  | optional_description UNION name directives EQUAL PIPE? union_vals
    {
      Union {
        name = $3;
        possibleVals = $7;
        directives = $4;
        description = $1
      }
    }

scalar_definition:
  | optional_description SCALAR name directives
    { Scalar { name = $3; directives = $4; description = $1 } }

schema_definition:
  | optional_description SCHEMA directives LBRACE root_field+ RBRACE
  { let fields = $5 in
    let field_names =
      List.map (fun ({name; _ }: Ast.schema_field) -> name) fields
    in
    if List.length field_names > 3 then
      raise (Validation_error
        "Schema definition node must not include more than 3 entries.")
    else if List.exists (fun name ->
      name <> "query" &&
      name <> "mutation" &&
      name <> "subscription") field_names then
      raise (Validation_error
        "Schema definition node must only include `query`, `mutation`, and `subscription` names.")
    else if has_duplicates field_names then
      raise (Validation_error
        "Schema definition node must not include duplicate names.")
    else
      Schema({ fields; directives = $3; description = $1 })
  }

%inline root_field:
  | QUERY COLON name
  {
    { name = "query";
      typ = $3;
    }
  }
  | MUTATION COLON name
  {
    { name = "mutation";
      typ = $3;
    }
  }
  | SUBSCRIPTION COLON name
  {
    { name = "subscription";
      typ = $3;
    }
  }

union_vals:
  | name  { [ {name = $1; description = None} ] }
  | name PIPE union_vals
  { {name = $1; description = None} :: $3 }

enum_vals:
  | LBRACE enum_value_decl+ RBRACE { $2 }

field_set:
  | LBRACE field+ RBRACE { $2 }

field:
  optional_description name default_list(arguments) COLON typ directives
    {
       {
        name = $2;
        typ = $5;
        arguments = $3;
        directives = $6;
        description = $1;
      }
    }

typ:
  | name { NamedType $1 }
  | LBRACK typ RBRACK { ListType $2 }
  | typ BANG { NonNullType $1 }

%inline directive:
  | AT name default_list(const_arguments)
    {
      {
        Ast.name = $2;
        arguments = $3;
      }
    }

%inline directives:
  | (* empty *) { [] }
  | directive+ { $1 }

%inline const_arguments:
  | LPAREN argument+ RPAREN { $2 }

%inline argument:
  | name COLON const_value { $1, $3 }

enum_value:
  | keyword_name
  | NAME { $1 }

enum_value_decl:
  | optional_description enum_value {
    {
      name = $2;
      description = $1;
    }
  }

keyword_name:
  | QUERY { "query" }
  | MUTATION { "mutation" }
  | SUBSCRIPTION { "subscription" }
  | INPUT { "input" }
  | INTERFACE { "interface" }
  | ENUM { "enum" }
  | UNION { "union" }
  | SCALAR { "scalar" }
  | SCHEMA { "schema" }
  (* | FRAGMENT { "fragment" } *)

fragment_name:
  | NULL { "null" }
  | BOOL { string_of_bool $1 }
  | NAME { $1 }
  | keyword_name { $1 }

name:
  | fragment_name { $1 }
  | ON { "on" }
  | IMPLEMENTS { "implements" }

arguments:
  | LPAREN argument_definition+ RPAREN { $2 }

argument_definition:
  | optional_description name COLON typ default_value? {
    {
      name = $2;
      typ = $4;
      default_value = $5;
      description = $1
    }
  }

%inline default_value:
  | EQUAL const_value { $2 }

%inline const_value:
  | NULL { `Null }
  | INT { `Int $1 }
  | FLOAT { `Float $1 }
  | STRING { `String $1 }
  | BOOL { `Bool $1 }
  | enum_value { `Enum $1 }
  | LBRACK const_value* RBRACK { `List $2 }
  | LBRACE list(name COLON const_value { $1, $3 }) RBRACE { `Assoc $2 }

