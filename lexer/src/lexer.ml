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
  let identifier =
    let alphanum_or_underscore =
      char_or (List.filter Char.all ~f:(fun c -> Char.is_alphanum c || Char.equal c '_'))
    in
    let alpha_or_underscore =
      char_or (List.filter Char.all ~f:(fun c -> Char.is_alpha c || Char.equal c '_'))
    in
    concat [ alpha_or_underscore; Star alphanum_or_underscore ]
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
  ; (* Symbol *)
    const_dfa (exact ".") (Token.Symbol (Non_custom Dot))
  ; const_dfa (exact ":") (Token.Symbol (Non_custom Colon))
  ; const_dfa (exact ";") (Token.Symbol (Non_custom Semicolon))
  ; const_dfa (exact ",") (Token.Symbol (Non_custom Comma))
  ; const_dfa (exact ":=") (Token.Symbol (Non_custom Walrus))
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
            | '=' -> Token.Symbol.Base.Equal
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
        Token.Symbol (Base base_symbols))
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

let lex input =
  let module To_process = struct
    module Text = struct
      type t =
        { input : string
        ; index : int
        }

      let create input = { input; index = 0 }
    end

    type t = Text.t list

    let rec next : t -> (t * char) option = function
      | { Text.input; index } :: rest_of_text ->
        if index >= String.length input
        then next rest_of_text
        else (
          let char = input.[index] in
          let t = { Text.input; index = index + 1 } :: rest_of_text in
          Some (t, char))
      | [] -> None
    ;;

    let prepend (t : t) ~(with_ : Text.t) = with_ :: t
  end
  in
  let rec loop_whitestring to_process =
    (let%map.Option next_to_process, c = To_process.next to_process in
     if Char.is_whitespace c
     then loop_whitestring next_to_process
     else on_non_whitespace to_process)
    |> Option.value ~default:(With_errors.return [])
  and on_non_whitespace to_process =
    let iterator = Regex_dfa.Iterator.make token_dfa in
    let rec loop to_process =
      (let%map.Option next_to_process, c = To_process.next to_process in
       match Regex_dfa.Iterator.next iterator ~c with
       | Complete { result; unused } ->
         let%map.With_errors next_results =
           loop_whitestring
             (To_process.prepend next_to_process ~with_:(To_process.Text.create unused))
         in
         result :: next_results
       | Failure { input } ->
         With_errors.error_s [] [%message "Failed to lex" (input : string)]
       | Incomplete -> loop next_to_process)
      |> Option.value ~default:(With_errors.return [])
    in
    loop to_process
  in
  loop_whitestring [ To_process.Text.create input ]
;;
