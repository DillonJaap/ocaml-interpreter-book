open! Core
open Lexer

let get_all_tokens lex =
  let rec aux lex token_list =
    let lex', token = next_token lex in
    match token with
    | EOF as token -> List.rev (token :: token_list)
    | token -> aux lex' (token :: token_list)
  in
  aux lex []

let print_tokens tokens =
  List.iter tokens ~f:(fun t -> printf "%s " (Token.as_string t))

(* different types of tests *)
let%expect_test "test lexer" =
  new_lexer {|test "test" 123 + = != == &|} |> get_all_tokens |> print_tokens;
  [%expect {|
    test "test" 123 + = != == Illegal EOF |}]

let%test_unit "test lexer" =
  [%test_eq: Token.t list]
    (new_lexer {|test "test"|} |> get_all_tokens)
    [ Token.Identifier "test"; Token.String "test"; Token.EOF ]

let lex =
  new_lexer
    {|let five = 5;
	let ten = 10;

	let add = fn(x, y) {
		x + y;
	};

	let result = add(five, ten);

	!-/*5;
	5 < 10 > 5;

	if (5 < 10) {
		return true;
	} else {
		return false;
	}

	10 == 10;
	10 != 9;
	"foobar"
	"foo bar"
	[1, 2];
	{"foo": "bar"}
    |}

let%test_unit "test lexer extensive" =
  [%test_eq: Token.t list] (get_all_tokens lex)
    [
      Token.Let;
      Token.Identifier "five";
      Token.Assign;
      Token.Integer 5;
      Token.Semicolon;
      Token.Let;
      Token.Identifier "ten";
      Token.Assign;
      Token.Integer 10;
      Token.Semicolon;
      Token.Let;
      Token.Identifier "add";
      Token.Assign;
      Token.Function;
      Token.Lparen;
      Token.Identifier "x";
      Token.Comma;
      Token.Identifier "y";
      Token.Rparen;
      Token.Lbrace;
      Token.Identifier "x";
      Token.Plus;
      Token.Identifier "y";
      Token.Semicolon;
      Token.Rbrace;
      Token.Semicolon;
      Token.Let;
      Token.Identifier "result";
      Token.Assign;
      Token.Identifier "add";
      Token.Lparen;
      Token.Identifier "five";
      Token.Comma;
      Token.Identifier "ten";
      Token.Rparen;
      Token.Semicolon;
      Token.Bang;
      Token.Minus;
      Token.Slash;
      Token.Asterisk;
      Token.Integer 5;
      Token.Semicolon;
      Token.Integer 5;
      Token.Less_Than;
      Token.Integer 10;
      Token.Greater_Than;
      Token.Integer 5;
      Token.Semicolon;
      Token.If;
      Token.Lparen;
      Token.Integer 5;
      Token.Less_Than;
      Token.Integer 10;
      Token.Rparen;
      Token.Lbrace;
      Token.Return;
      Token.True;
      Token.Semicolon;
      Token.Rbrace;
      Token.Else;
      Token.Lbrace;
      Token.Return;
      Token.False;
      Token.Semicolon;
      Token.Rbrace;
      Token.Integer 10;
      Token.Equal;
      Token.Integer 10;
      Token.Semicolon;
      Token.Integer 10;
      Token.Not_Equal;
      Token.Integer 9;
      Token.Semicolon;
      Token.String "foobar";
      Token.String "foo bar";
      Token.Lbracket;
      Token.Integer 1;
      Token.Comma;
      Token.Integer 2;
      Token.Rbracket;
      Token.Semicolon;
      Token.Lbrace;
      Token.String "foo";
      Token.Colon;
      Token.String "bar";
      Token.Rbrace;
      Token.EOF;
    ]
