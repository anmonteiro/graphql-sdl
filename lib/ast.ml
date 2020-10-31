exception Validation_error of string

type directive =
  { name : string
  ; arguments : (string * Graphql_parser.const_value) list
  }

type argument_definition =
  { name : string
  ; typ : Graphql_parser.typ
  ; default_value : Graphql_parser.const_value option
  ; description : string option
  }

type field =
  { typ : Graphql_parser.typ
  ; name : string
  ; arguments : argument_definition list
  ; directives : directive list
  ; description : string option
  }

type schemaTyp =
  { name : string
  ; field_defs : field list
  ; directives : directive list
  ; description : string option
  ; interfaces : string list option
  }

type enumValue =
  { name : string
  ; description : string option
  }

type enumTyp =
  { name : string
  ; possibleVals : enumValue list
  ; directives : directive list
  ; description : string option
  }

type scalar_definition =
  { name : string
  ; directives : directive list
  ; description : string option
  }

type schema_field =
  { typ : string
  ; name : string
  }

type schema_definition =
  { fields : schema_field list
  ; directives : directive list
  ; description : string option
  }

type optype =
  | InputType of schemaTyp
  | Type of schemaTyp
  | Enum of enumTyp
  | Union of enumTyp
  | Interface of schemaTyp
  | Scalar of scalar_definition
  | Schema of schema_definition
