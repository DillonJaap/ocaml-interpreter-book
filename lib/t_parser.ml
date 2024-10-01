open! Core

exception MustBeProgram of string

let new_test_parser input =
  let node = Lexer.new_lexer input |> Parser.new_parser |> Parser.parse in
  match node with
  | Ast.Program n -> n
  | _ -> raise (MustBeProgram "error expected Ast.Program")

let%test_unit "test let statement" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|let five = 5; let five = "5"; let foo = bar;|})
    [
      Ast.Let { name = "five"; value = Ast.Integer_Literal 5 };
      Ast.Let { name = "five"; value = Ast.String_Literal "5" };
      Ast.Let { name = "foo"; value = Ast.Identifier "bar" };
    ]

let%test_unit "test infix and prefix expressions" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|5 + 5; let x = 5 + 8 * 6;true;|})
    [
      Ast.Expression_Statement
        (Ast.Infix_Expression
           {
             left = Ast.Integer_Literal 5;
             operator = "+";
             right = Ast.Integer_Literal 5;
           });
      Ast.Let
        {
          name = "x";
          value =
            Ast.Infix_Expression
              {
                left = Ast.Integer_Literal 5;
                operator = "+";
                right =
                  Ast.Infix_Expression
                    {
                      left = Ast.Integer_Literal 8;
                      operator = "*";
                      right = Ast.Integer_Literal 6;
                    };
              };
        };
      Ast.Expression_Statement (Ast.Boolean true);
    ]

let%test_unit "test return statement" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|return "TODO temp";|})
    [ Ast.Return (Ast.String_Literal "TODO temp") ]

let%test_unit "test call expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|test(arg, 1, 1 + 1); test()|})
    [
      Ast.Expression_Statement
        (Ast.Call_Expression
           {
             function_ = Ast.Identifier "test";
             arguments =
               [
                 Ast.Identifier "arg";
                 Ast.Integer_Literal 1;
                 Ast.Infix_Expression
                   {
                     left = Ast.Integer_Literal 1;
                     operator = "+";
                     right = Ast.Integer_Literal 1;
                   };
               ];
           });
      Ast.Expression_Statement
        (Ast.Call_Expression
           { function_ = Ast.Identifier "test"; arguments = [] });
    ]

let%test_unit "test index expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|test[5];|})
    [
      Ast.Expression_Statement
        (Ast.Index_Expression
           { left = Ast.Identifier "test"; index = Ast.Integer_Literal 5 });
    ]

let%test_unit "test array and hash literal" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|[];[2];[1, 2];{"one": 1, "two": 2}|})
    [
      Ast.Expression_Statement (Ast.Array_Literal []);
      Ast.Expression_Statement (Ast.Array_Literal [ Ast.Integer_Literal 2 ]);
      Ast.Expression_Statement
        (Ast.Array_Literal [ Ast.Integer_Literal 1; Ast.Integer_Literal 2 ]);
      Ast.Expression_Statement
        (Ast.Hash_Literal
           [
             (Ast.String_Literal "one", Ast.Integer_Literal 1);
             (Ast.String_Literal "two", Ast.Integer_Literal 2);
           ]);
    ]

let%test_unit "test function literal" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|fn(x, y) { x + y; }|})
    [
      Ast.Expression_Statement
        (Ast.Function_Literal
           {
             parameters = [ "x"; "y" ];
             body =
               [
                 Ast.Expression_Statement
                   (Ast.Infix_Expression
                      {
                        left = Ast.Identifier "x";
                        operator = "+";
                        right = Ast.Identifier "y";
                      });
               ];
           });
    ]

let%test_unit "test if expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|if (x < y) { x }|})
    [
      Ast.Expression_Statement
        (Ast.If_Expression
           {
             condition =
               Ast.Infix_Expression
                 {
                   left = Ast.Identifier "x";
                   operator = "<";
                   right = Ast.Identifier "y";
                 };
             consequence = [ Ast.Expression_Statement (Ast.Identifier "x") ];
             alternate = [];
           });
    ]

let%test_unit "test if else expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|if (x < y) { x } else { y }|})
    [
      Ast.Expression_Statement
        (Ast.If_Expression
           {
             condition =
               Ast.Infix_Expression
                 {
                   left = Ast.Identifier "x";
                   operator = "<";
                   right = Ast.Identifier "y";
                 };
             consequence = [ Ast.Expression_Statement (Ast.Identifier "x") ];
             alternate = [ Ast.Expression_Statement (Ast.Identifier "y") ];
           });
    ]
