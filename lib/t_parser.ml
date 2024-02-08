open! Core
open Parser

let new_test_parser input = parse @@ new_parser @@ Lexer.new_lexer input

let%test_unit "test let statement" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|let five = 5; let five = "5"; let foo = bar;|})
    [
      Ast.Let { name = "five"; value = Ast.Integer_Literal { value = 5 } };
      Ast.Let { name = "five"; value = Ast.String_Literal { value = "5" } };
      Ast.Let { name = "foo"; value = Ast.Identifier { value = "bar" } };
    ]

let%test_unit "test infix expressions" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|5 + 5; let x = 5 + 8 * 6;|})
    [
      Ast.Expression_Statement
        {
          value =
            Ast.Infix_Expression
              {
                left = Ast.Integer_Literal { value = 5 };
                operator = "+";
                right = Ast.Integer_Literal { value = 5 };
              };
        };
      Ast.Let
        {
          name = "x";
          value =
            Ast.Infix_Expression
              {
                left = Ast.Integer_Literal { value = 5 };
                operator = "+";
                right =
                  Ast.Infix_Expression
                    {
                      left = Ast.Integer_Literal { value = 8 };
                      operator = "*";
                      right = Ast.Integer_Literal { value = 6 };
                    };
              };
        };
    ]

let%test_unit "test return statement" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|return "TODO temp";|})
    [ Ast.Return { value = Ast.String_Literal { value = "TODO temp" } } ]

let%test_unit "test call expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|test(arg, 1, 1 + 1)|})
    [
      Ast.Expression_Statement
        {
          value =
            Ast.Call_Expression
              {
                function_ = Ast.Identifier { value = "test" };
                arguments =
                  [
                    Ast.Identifier { value = "arg" };
                    Ast.Integer_Literal { value = 1 };
                    Ast.Infix_Expression
                      {
                        left = Ast.Integer_Literal { value = 1 };
                        operator = "+";
                        right = Ast.Integer_Literal { value = 1 };
                      };
                  ];
              };
        };
    ]
