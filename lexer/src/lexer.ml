open! Core

let token_dfa =
  let open Regex_dfa.Config in
  let num =
    let char_or_digit = List.filter Char.all ~f:Char.is_digit in
    char_or char_or_digit
  in
  let single_char without =
    let backslash = '\\' in
    let escaped = Concat (Char backslash, char_or Char.all) in
    let char_or_unescaped =
      List.filter Char.all ~f:(fun c ->
        not (List.exists (backslash :: without) ~f:(Char.equal c)))
    in
    Or (escaped, char_or char_or_unescaped)
  in
  let identifier, big_identifier =
    let alphanum_or_underscore =
      char_or (List.filter Char.all ~f:(fun c -> Char.is_alphanum c || Char.equal c '_'))
    in
    let is_uppercase_alpha c = Char.is_alpha c && Char.is_uppercase c in
    let is_lowercase_alpha c = Char.is_alpha c && Char.is_lowercase c in
    let lower_alpha_or_underscore =
      char_or
        (List.filter Char.all ~f:(fun c -> is_lowercase_alpha c || Char.equal c '_'))
    in
    let upper_alpha_or_underscore =
      char_or
        (List.filter Char.all ~f:(fun c -> is_uppercase_alpha c || Char.equal c '_'))
    in
    ( concat [ lower_alpha_or_underscore; Star alphanum_or_underscore ]
    , concat [ upper_alpha_or_underscore; Star alphanum_or_underscore ] )
  in
  let with_transformation ?keyword () =
    let prefix =
      match keyword with
      | Some keyword -> keyword ^ "#"
      | None -> "#"
    in
    concat [ exact prefix; identifier ]
  in
  let buffer_suffix buffer ~pos =
    Buffer.sub buffer ~pos ~len:(Buffer.length buffer - pos)
  in
  let const_dfa config token = Regex_dfa.create config ~cont_of_match:(Fn.const token) in
  let text_dfa content ~constructor ~delimiter =
    Regex_dfa.create
      (concat [ Char delimiter; content; Char delimiter ])
      ~cont_of_match:(fun buffer ->
        let bytes = Buffer.sub buffer ~pos:1 ~len:(Buffer.length buffer - 2) in
        constructor (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes))
  in
  let with_transformation_dfa ?keyword () ~constructor =
    let keyword_len =
      Option.value_map keyword ~default:1 ~f:(fun keyword -> String.length keyword + 1)
    in
    Regex_dfa.create (with_transformation ?keyword ()) ~cont_of_match:(fun buffer ->
      let name =
        Bytes.unsafe_to_string
          ~no_mutation_while_string_reachable:(buffer_suffix buffer ~pos:keyword_len)
      in
      constructor { Token.Identifier.name })
  in
  [ (* Constant *)
    const_dfa (exact "true") (Token.Constant (Bool true))
  ; const_dfa (exact "false") (Token.Constant (Bool false))
  ; Regex_dfa.create (plus num) ~cont_of_match:(fun buffer ->
      Token.Constant (Int (Buffer.contents buffer)))
  ; Regex_dfa.create
      (Or
         (concat [ plus num; Char '.'; Star num ], concat [ Star num; Char '.'; plus num ]))
      ~cont_of_match:(fun buffer -> Token.Constant (Float (Buffer.contents buffer)))
  ; text_dfa
      (single_char [ '\'' ])
      ~constructor:(fun str -> Token.Constant (Char str))
      ~delimiter:'\''
  ; text_dfa
      (Star (single_char [ '\"' ]))
      ~constructor:(fun str -> Token.Constant (String str))
      ~delimiter:'\"'
  ; (* Identifier *)
    Regex_dfa.create ~priority:0 identifier ~cont_of_match:(fun buffer ->
      let name = Buffer.contents buffer in
      Token.Identifier { name })
  ; (* Big_identifier *)
    Regex_dfa.create ~priority:0 big_identifier ~cont_of_match:(fun buffer ->
      let name = Buffer.contents buffer in
      Token.Big_identifier { name })
  ; (* Symbol *)
    const_dfa (exact "::") (Token.Symbol (Operator (Non_custom Double_colon)))
  ; const_dfa (exact ",") (Token.Symbol Comma)
  ; const_dfa (exact ":") (Token.Symbol Colon)
  ; const_dfa (exact "!") (Token.Symbol Bang)
  ; const_dfa (exact ";") (Token.Symbol Semicolon)
  ; const_dfa (exact ":=") (Token.Symbol Walrus)
  ; Regex_dfa.create
      (plus
         (char_or
            [ '='; '~'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '>'; '<' ]))
      ~cont_of_match:(fun buffer ->
        let base_symbols =
          Buffer.contents buffer
          |> String.to_list
          |> Nonempty_list.of_list_exn
          |> Nonempty_list.map ~f:(function
            | '.' -> Token.Symbol.Operator.Base.Dot
            | '=' -> Equal
            | '~' -> Tilda
            | '@' -> At
            | '^' -> Caret
            | '|' -> Pipe
            | '&' -> Ampersand
            | '+' -> Plus
            | '-' -> Minus
            | '*' -> Times
            | '/' -> Div
            | '$' -> Dollar
            | '%' -> Percent
            | '>' -> Greater
            | '<' -> Less
            | c -> raise_s [%message "Unexpected base symbol" (c : char)])
        in
        Token.Symbol (Operator (Base base_symbols)))
  ; (* Definition *)
    with_transformation_dfa () ~constructor:(fun id ->
      Token.Definition (Assign_with_transformation id))
  ; const_dfa (exact "rec") (Token.Definition Rec)
  ; const_dfa (exact "and") (Token.Definition And)
  ; const_dfa (exact "in") (Token.Definition In)
  ; const_dfa (exact "module") (Token.Definition (Module None))
  ; with_transformation_dfa ~keyword:"module" () ~constructor:(fun id ->
      Token.Definition (Module (Some id)))
  ; const_dfa (exact "sig") (Token.Definition Sig)
  ; const_dfa (exact "struct") (Token.Definition Struct)
  ; const_dfa (exact "end") (Token.Definition End)
  ; (* Conditional*)
    const_dfa (exact "if") (Token.Conditional (If None))
  ; with_transformation_dfa ~keyword:"if" () ~constructor:(fun id ->
      Token.Conditional (If (Some id)))
  ; const_dfa (exact "then") (Token.Conditional Then)
  ; const_dfa (exact "else") (Token.Conditional Else)
  ; const_dfa (exact "match") (Token.Conditional (Match None))
  ; with_transformation_dfa ~keyword:"match" () ~constructor:(fun id ->
      Token.Conditional (Match (Some id)))
  ; const_dfa (exact "when") (Token.Conditional When)
  ; const_dfa (exact "function") (Token.Conditional Function)
  ; (* Typeful *)
    const_dfa (exact "type") (Token.Typeful Type)
  ; const_dfa (exact "as") (Token.Typeful As)
  ; const_dfa (exact "of") (Token.Typeful Of)
  ; const_dfa (exact "mutable") (Token.Typeful Mutable)
  ; const_dfa (exact "nonrec") (Token.Typeful Nonrec)
  ; (* Typeful *)
    const_dfa (exact "(") (Token.Grouping (Parenthesis Left))
  ; const_dfa (exact ")") (Token.Grouping (Parenthesis Right))
  ; const_dfa (exact "{") (Token.Grouping (Curly_bracket Left))
  ; const_dfa (exact "}") (Token.Grouping (Curly_bracket Right))
  ; const_dfa (exact "[") (Token.Grouping (Square_bracket Left))
  ; const_dfa (exact "]") (Token.Grouping (Square_bracket Right))
  ; (* Import *)
    const_dfa (exact "open") (Token.Import (Open { allow_unused = false }))
  ; const_dfa (exact "open!") (Token.Import (Open { allow_unused = true }))
  ; const_dfa (exact "include") (Token.Import Include)
  ; (* Misc *)
    const_dfa (exact "lambda") Token.Lambda
  ; const_dfa (exact "functor") Token.Functor
  ; const_dfa (exact "with") Token.With
  ]
  |> Regex_dfa.merge_list
;;

let lex input ~filename =
  let module To_process = struct
    type t =
      { input : string
      ; index : int
      ; start : Source_position.Within_file.t
      ; current : Source_position.Within_file.t
      ; last_accepting_state_index_and_position :
          (* TODO: this should be a labelled tuple *)
          (int * Source_position.Within_file.t) option
      }
    [@@deriving sexp_of]

    let create input =
      { input
      ; index = 0
      ; start = { line_number = 0; column_number = 0 }
      ; current = { line_number = 0; column_number = 0 }
      ; last_accepting_state_index_and_position = None
      }
    ;;

    let next
          ?(start_is_current = false)
          ({ input
           ; index
           ; start
           ; current = { line_number; column_number }
           ; last_accepting_state_index_and_position = _
           } as t)
      =
      if index < String.length input
      then (
        let char = input.[index] in
        let (current : Source_position.Within_file.t) =
          match char with
          | '\n' -> { line_number = line_number + 1; column_number = 0 }
          | (_ : char) -> { line_number; column_number = column_number + 1 }
        in
        Some
          ( { t with
              index = index + 1
            ; start = (if start_is_current then current else start)
            ; current
            }
          , char ))
      else None
    ;;

    let update_last_accepting_state t =
      { t with last_accepting_state_index_and_position = Some (t.index, t.current) }
    ;;

    let result_and_reset t ~result =
      let%map.Or_error index, source_position =
        Or_error.of_option
          t.last_accepting_state_index_and_position
          ~error:
            (Error.create_s
               [%message "Applying last accepting state when none exists" (t : t)])
      in
      let result =
        { Source_position.With_section.value = result
        ; filename
        ; start = t.start
        ; end_ = source_position
        }
      in
      (* TODO this should be a labelled tuple *)
      ( result
      , { t with
          index
        ; start = source_position
        ; last_accepting_state_index_and_position = None
        ; current = source_position
        } )
    ;;
  end
  in
  let rec loop_whitestring to_process =
    (let%map.Option next_to_process, c =
       To_process.next ~start_is_current:true to_process
     in
     if Char.is_whitespace c
     then loop_whitestring next_to_process
     else on_non_whitespace to_process)
    |> Option.value ~default:(With_errors.return [])
  and on_non_whitespace to_process =
    let iterator = Regex_dfa.Iterator.make token_dfa in
    let rec loop to_process =
      (let%map.Option next_to_process, c = To_process.next to_process in
       match Regex_dfa.Iterator.next iterator ~c with
       | Complete { result; unused_len = _ } ->
         (match To_process.result_and_reset next_to_process ~result with
          | Error error -> With_errors.create_error [] error
          | Ok (result, next_to_process) ->
            let%map.With_errors next_results = loop_whitestring next_to_process in
            result :: next_results)
       | Failure { input } ->
         With_errors.error_s [] [%message "Failed to lex" (input : string)]
       | Incomplete { is_accepting_state = false } -> loop next_to_process
       | Incomplete { is_accepting_state = true } ->
         loop (To_process.update_last_accepting_state next_to_process))
      |> Option.value ~default:(With_errors.return [])
    in
    loop to_process
  in
  loop_whitestring (To_process.create input)
;;
