open Easy_format

type break_criterion =
  | Never
  | IfNeed
  | Always
  | Always_rec

let easyLabel ?(break = `Auto) ?(space = false) ?(indent = 0) labelTerm term =
  let settings =
    { label_break = break
    ; space_after_label = space
    ; indent_after_label = indent
    ; label_style = Some "label"
    }
  in
  Easy_format.Label ((labelTerm, settings), term)

let label
    ?break ?space ?indent (labelTerm : Easy_format.t) (term : Easy_format.t)
  =
  easyLabel ?break ?indent ?space labelTerm term

let atom str =
  let style = { Easy_format.atom_style = Some "atomClss" } in
  Atom (str, style)

let makeList
    ?(break = IfNeed)
    ?(preSpace = false)
    ?(space = true)
    ?(indent = 2)
    ?(inline = true, false)
    ?(wrap = "", "")
    ?(sep = "")
    nodes
  =
  let inlineStart, inlineEnd = inline in
  let easy_break =
    match break with
    | Never ->
      `No_breaks
    | IfNeed ->
      `Never_wrap
    | Always ->
      `Force_breaks
    | Always_rec ->
      `Force_breaks_rec
  in
  let style =
    { Easy_format.list with
      space_after_opening = false
    ; space_before_separator = preSpace
    ; space_after_separator = space
    ; space_before_closing = false
    ; wrap_body = easy_break
    ; indent_body = indent
    ; stick_to_label = inlineStart
    ; align_closing = not inlineEnd
    }
  in
  let opn, cls = wrap in
  List ((opn, sep, cls, style), nodes)

let rec formatFieldTyp typ =
  match typ with
  | Graphql_parser.NamedType x ->
    x
  | ListType t ->
    "[" ^ formatFieldTyp t ^ "]"
  | NonNullType t ->
    formatFieldTyp t ^ "!"

let rec formatConstValue value =
  match value with
  | `Null ->
    atom "null"
  | `Int x ->
    atom (string_of_int x)
  | `Float x ->
    atom (string_of_float x)
  | `Enum x ->
    atom x
  | `String x ->
    atom ("\"" ^ x ^ "\"")
  | `Bool x ->
    atom (string_of_bool x)
  | `List xs ->
    makeList ~sep:"," (List.map formatConstValue xs)
  | `Assoc xs ->
    makeList
      ~wrap:("{", "}")
      (List.map
         (fun (k, v) -> makeList ~sep:":" [ atom k; formatConstValue v ])
         xs)

let formatDescription ?(inline = true) description layout =
  match description with
  | None ->
    layout
  | Some description ->
    let description_layout =
      makeList
        ~break:Never
        ~space:false
        [ atom "\""; atom description; atom "\"" ]
    in
    makeList
      ~indent:0
      ~space:false
      ~inline:
        (if inline then
           true, true
        else
          true, false)
      ~break:Always
      [ description_layout; layout ]

let formatArguments arguments =
  match arguments with
  | [] ->
    None
  | xs ->
    let args_len = List.length arguments in
    let args_layout =
      makeList
        ~inline:(true, true)
        (List.mapi
           (fun i { Ast.name; typ; default_value; description } ->
             let argDef =
               makeList
                 [ makeList ~space:false [ atom name; atom ":" ]
                 ; (if i == args_len - 1 then
                      atom (formatFieldTyp typ)
                   else
                     makeList
                       ~break:Never
                       ~space:false
                       [ atom (formatFieldTyp typ); atom "," ])
                 ]
             in
             let break =
               match description with None -> Never | Some _ -> Always_rec
             in
             let arg_def_layout =
               match default_value with
               | None ->
                 argDef
               | Some value ->
                 makeList [ argDef; atom "="; formatConstValue value ]
             in
             formatDescription
               description
               (makeList
                  ~inline:(true, true)
                  ~break
                  ~space:false
                  [ arg_def_layout ]))
           xs)
    in
    Some args_layout

let format_directives directives =
  List.map
    (fun ({ name; arguments } : Ast.directive) ->
      let arguments_layout =
        makeList
          ~sep:","
          ~wrap:("(", ")")
          (List.map
             (fun (name, value) ->
               makeList
                 [ makeList ~space:false [ atom name; atom ":" ]
                 ; formatConstValue value
                 ])
             arguments)
      in
      makeList
        ~space:false
        ([ atom "@"; atom name ]
        @
        if arguments == [] then
          []
        else
          [ arguments_layout ]))
    directives

let formatFields fields =
  List.map
    (fun { Ast.typ; name; arguments; description; directives } ->
      let layout =
        match formatArguments arguments with
        | None ->
          atom name
        | Some args_layout ->
          let wrapped = makeList ~wrap:("(", ")") [ args_layout ] in
          label ~break:`Never (atom name) wrapped
      in
      let field_layout =
        makeList
          ~space:false
          ~indent:0
          ~break:Never
          [ layout; makeList [ atom ":"; atom (formatFieldTyp typ) ] ]
      in
      formatDescription
        description
        (makeList
           ~inline:(true, true)
           ~indent:0
           (field_layout :: format_directives directives)))
    fields

let formatEnumVals enumVals =
  List.map
    (fun ({ name; description } : Ast.enumValue) ->
      formatDescription description (atom name))
    enumVals

let formatSchemaFields fields =
  List.map
    (fun { Ast.name; typ } ->
      makeList ~space:false [ atom name; makeList [ atom ":"; atom typ ] ])
    fields

let formatUnion unionDef possibleVals description =
  let union =
    makeList
      [ unionDef
      ; atom "="
      ; makeList
          ~preSpace:true
          ~sep:"|"
          (List.map
             (fun ({ name; _ } : Ast.enumValue) -> atom name)
             possibleVals)
      ]
  in
  formatDescription description union

let formatType typeDef fields description =
  let typ =
    label ~space:true typeDef (makeList ~break:Always ~wrap:("{", "}") fields)
  in
  formatDescription description typ

let pprint_typ typ =
  match typ with
  | Ast.Type { name; field_defs; directives; description; interfaces } ->
    let intfs_layout =
      match interfaces with
      | None ->
        []
      | Some intfs ->
        List.concat
          (List.map (fun intf -> [ atom "implements"; atom intf ]) intfs)
    in
    formatType
      (label
         ~space:true
         (atom "type")
         (makeList
            (List.concat
               [ [ atom name ]; intfs_layout; format_directives directives ])))
      (formatFields field_defs)
      description
  | InputType { name; field_defs; directives; description; _ } ->
    formatType
      (label
         ~space:true
         (atom "input")
         (makeList (atom name :: format_directives directives)))
      (formatFields field_defs)
      description
  | Enum { name; possibleVals; directives; description } ->
    formatType
      (label
         ~space:true
         (atom "enum")
         (makeList (atom name :: format_directives directives)))
      (formatEnumVals possibleVals)
      description
  | Union { name; possibleVals; directives; description } ->
    formatUnion
      (label
         ~space:true
         (atom "union")
         (makeList (atom name :: format_directives directives)))
      possibleVals
      description
  | Interface { name; field_defs; directives; description; _ } ->
    formatType
      (label
         ~space:true
         (atom "interface")
         (makeList (atom name :: format_directives directives)))
      (formatFields field_defs)
      description
  | Scalar { name; directives; description } ->
    let scalar =
      label
        ~space:true
        (atom "scalar")
        (makeList (atom name :: format_directives directives))
    in
    formatDescription description scalar
  | Schema { fields; directives; description } ->
    formatType
      (makeList ~space:true (atom "schema" :: format_directives directives))
      (formatSchemaFields fields)
      description

let pprint_schema schema =
  makeList ~break:Always ~sep:"\n" ~indent:0 (List.map pprint_typ schema)
