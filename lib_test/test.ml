(*
 * Copyright (c) 2018 Neelakantan Krishnaswami and Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2

module SC = Asp.Staged.Parse(Asp_utilities.Char_element)
open SC

module StringParser = Parser(Asp_streamcode.Stringcode)
module ChannelParser = Parser(Asp_streamcode.Channelcode)


let build_parser m = Runnative.run (StringParser.compile m)
let build_channel_parser m = Runnative.run (ChannelParser.compile m)

let test_digits _ =
  let digit = Asp.Utilities.Staged.Charparsing.digit
              $ fun x -> .< Char.code .~x - Char.code '0' >. in

  (* digits with 'star' *)
  let digits' = type_check @@ (star digit) in

  (* digits with 'fix' *)
  let digits = type_check @@
    fix @@ fun d ->
    (digit >>> d $ fun dds -> .< fst .~dds :: snd .~dds >.)
    <|> eps .<[]>. in

  let parser1 = build_parser digits ~index:0
  and parser2 = build_parser digits' ~index:0
  in
  begin
    assert_equal ([1]) (fst @@ parser1 "1b3");
    assert_equal ([1]) (fst @@ parser2 "1b3");

    assert_equal ([]) (fst @@ parser1 "");
    assert_equal ([]) (fst @@ parser2 "");

    assert_equal ([1; 0; 1; 3; 4; 5]) (fst @@ parser1 "101345");
    assert_equal ([1; 0; 1; 3; 4; 5]) (fst @@ parser2 "101345");
  end


let test_abab _ =
  let abab = type_check @@ star (tok (Chr 'a') >>> tok (Chr 'b')) in
  let parser = build_parser abab ~index:0 in
  begin
    assert_equal (['a','b'; 'a','b']) (fst @@ parser "ababcde");
    
    assert_equal ([]) (fst @@ parser "");
  end


let bracket =
  let unit _ = .<()>. in
  type_check @@
  fix @@ fun d ->
         (((tok (Chr '(') $ unit) >>>
             (d $ unit) >>>
             (tok (Chr ')') $ unit))  $ fun _ -> .<true>.)
         <|>
           (eps .< false >.)


let test_bracket _ =
  let parser = build_parser bracket ~index:0 in
  begin
    assert_equal (false) (fst @@ parser "");
    
    assert_equal (true) (fst @@ parser "()");
    
    assert_equal (true) (fst @@ parser "((((()))))");
    
    assert_raises (Failure "Expected chr") @@ fun () ->
    ignore (parser "((((())))")
  end


let test_bracket_channel _ =
  let parser = build_channel_parser bracket in
  let test_channel filename test =
    let fd = open_in filename in
    match test fd with
      x -> close_in fd; x
    | exception e -> close_in fd; raise e
  in
  begin
    begin
      test_channel "brackets.ok" @@ fun fd ->
      assert_equal (true) (parser fd)
    end;
    
    begin
      test_channel "brackets.bad" @@ fun fd ->
      assert_raises (Failure "Expected chr") @@ fun () ->
      ignore (parser fd)
    end;
  end


let unstaged_intexp _ =
  let parse = Test_int_exp.Exp.parse_exp in
  let open Test_int_exp.Tok in
  let cases = Test_int_exp.Exp.[
      "x", Var "x";

      "1", Int 1;

      "(x)", Var "x";

      "x + y", Op (Plus, Var "x", Var "y");

      "x * y", Op (Times, Var "x", Var "y");

      "x + y * z", Op (Plus, Var "x", Op (Times, Var "y", Var "z"));

      "(x + y) * z", Op (Times, Op (Plus, Var "x", Var "y"), Var "z");

      "let x = 5 in z", Let ("x", Int 5, Var "z");

      "let a = x + y * z in\n \
       let b = (x + y) * z in\n \
       a + b",
      Let ("a", Op (Plus, Var "x", Op (Times, Var "y", Var "z")),
           Let ("b", Op (Times, Op (Plus, Var "x", Var "y"), Var "z"),
                Op (Plus, Var "a", Var "b")));

      "x = y",
      Op (Eql, Var "x", Var "y");

      "if (x = 0) then x else y",
      If (Op (Eql, Var "x", Int 0),
          Var "x",
          Var "y");

      "if (x = 0) then\n \
         let a = 6 in\n \
         y + a\n \
       else\n \
         let b = 7 in\n \
       b",
      If (Op (Eql, Var "x", Int 0),
          Let ("a", Int 6,
               Op (Plus, Var "y", Var "a")),
          Let ("b", Int 7,
               Var "b"));
    ]
  in
  ListLabels.iter cases
    ~f:(fun (s, ast) ->
      assert_equal ~msg:s
        ast
        (parse s))
  

let suite = "Parser tests" >:::
  ["digits"
    >:: test_digits;

   "abab"
    >:: test_abab;

   "bracket"
    >:: test_bracket;

   "unstaged intexp"
   >:: unstaged_intexp;

   "channel"
   >:: test_bracket_channel;
  ]


let _ =
  run_test_tt_main suite
