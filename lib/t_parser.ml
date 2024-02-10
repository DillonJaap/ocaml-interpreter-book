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

let%test_unit "test infix and prefix expressions" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|5 + 5; let x = 5 + 8 * 6;true;|})
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
      Ast.Expression_Statement { value = Ast.Boolean { value = true } };
    ]

let%test_unit "test return statement" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|return "TODO temp";|})
    [ Ast.Return { value = Ast.String_Literal { value = "TODO temp" } } ]

let%test_unit "test call expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|test(arg, 1, 1 + 1); test()|})
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
      Ast.Expression_Statement
        {
          value =
            Ast.Call_Expression
              { function_ = Ast.Identifier { value = "test" }; arguments = [] };
        };
    ]

let%test_unit "test index expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|test[5];|})
    [
      Ast.Expression_Statement
        {
          value =
            Ast.Index_Expression
              {
                left = Ast.Identifier { value = "test" };
                index = Ast.Integer_Literal { value = 5 };
              };
        };
    ]

let%test_unit "test array and hash literal" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|[];[2];[1, 2];{"one": 1, "two": 2}|})
    [
      Ast.Expression_Statement { value = Ast.Array_Literal { value = [] } };
      Ast.Expression_Statement
        {
          value =
            Ast.Array_Literal { value = [ Ast.Integer_Literal { value = 2 } ] };
        };
      Ast.Expression_Statement
        {
          value =
            Ast.Array_Literal
              {
                value =
                  [
                    Ast.Integer_Literal { value = 1 };
                    Ast.Integer_Literal { value = 2 };
                  ];
              };
        };
      Ast.Expression_Statement
        {
          value =
            Ast.Hash_Literal
              [
                ( Ast.String_Literal { value = "one" },
                  Ast.Integer_Literal { value = 1 } );
                ( Ast.String_Literal { value = "two" },
                  Ast.Integer_Literal { value = 2 } );
              ];
        };
    ]

let%test_unit "test function literal" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|fn(x, y) { x + y; }|})
    [
      Ast.Expression_Statement
        {
          value =
            Ast.Function_Literal
              {
                parameters = [ "x"; "y" ];
                body =
                  [
                    Ast.Expression_Statement
                      {
                        value =
                          Ast.Infix_Expression
                            {
                              left = Ast.Identifier { value = "x" };
                              operator = "+";
                              right = Ast.Identifier { value = "y" };
                            };
                      };
                  ];
              };
        };
    ]

let%test_unit "test if expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|if (x < y) { x }|})
    [
      Ast.Expression_Statement
        {
          value =
            Ast.If_Expression
              {
                condition =
                  Ast.Infix_Expression
                    {
                      left = Ast.Identifier { value = "x" };
                      operator = "<";
                      right = Ast.Identifier { value = "y" };
                    };
                consequence =
                  [
                    Ast.Expression_Statement
                      { value = Ast.Identifier { value = "x" } };
                  ];
                alternate = [];
              };
        };
    ]

let%test_unit "test if else expression" =
  [%test_eq: Ast.statement list]
    (new_test_parser {|if (x < y) { x } else { y }|})
    [
      Ast.Expression_Statement
        {
          value =
            Ast.If_Expression
              {
                condition =
                  Ast.Infix_Expression
                    {
                      left = Ast.Identifier { value = "x" };
                      operator = "<";
                      right = Ast.Identifier { value = "y" };
                    };
                consequence =
                  [
                    Ast.Expression_Statement
                      { value = Ast.Identifier { value = "x" } };
                  ];
                alternate =
                  [
                    Ast.Expression_Statement
                      { value = Ast.Identifier { value = "y" } };
                  ];
              };
        };
    ]
