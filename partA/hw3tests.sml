(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["A","b","C"] = ["A","C"]
val test1b = only_capitals [] = []
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 ["A","bc","cd"] = "bc"
val test2b = longest_string1 [] = ""
val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["A","bc","cd"] = "cd"
val test3b = longest_string2 [] = ""

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4c = longest_string3 ["A","bc","cd"] = "bc"
val test4d = longest_string3 [] = ""

val test4b = longest_string4 ["A","B","C"] = "C"
val test4f = longest_string4 [] = ""

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5a = longest_capitalized ["A","bc","Cb"] = "Cb"
val test5b = longest_capitalized [] = ""

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7a = ((first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3]) handle NoAnswer => 10) = 10
val test7b = ((first_answer (fn x => if x > 3 then SOME x else NONE) []) handle NoAnswer => 10) = 10

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8b = all_answers (fn x => if x = 4 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8c = all_answers (fn x => if x > 4 then SOME [x] else NONE) [5,6] = SOME [6,5]

val test9a = count_wildcards Wildcard = 1
val test9d = count_wildcards (Variable "1") = 0
val test9g = count_wildcards (TupleP [Variable "1",Wildcard]) = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9e = count_wild_and_variable_lengths (TupleP [Variable("a"),Wildcard]) = 2
val test9f = count_wild_and_variable_lengths (TupleP [Variable("a"),Wildcard,Variable("ab")]) = 4

val test9c = count_some_var ("x", Variable("x")) = 1
val test9h = count_some_var ("x", Variable("y")) = 0
val test9i = count_some_var ("x", TupleP [Variable("y"),Variable("x")]) = 1
val test9j = count_some_var ("x", TupleP [Variable("x"),Variable("x")]) = 2
val test9k = count_some_var ("x", TupleP [Variable("x"),Variable("x"),Wildcard]) = 2

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Variable("x"),Variable("x")]) = false
val test10b = check_pat (TupleP [Variable("x"),Variable("y")]) = true
val test10c = check_pat (TupleP [Variable("x"),Variable("y"),Wildcard]) = true

val test11 = match (Const(1), UnitP) = NONE
val test11a = match (Const(1), ConstP(2)) = NONE
val test11b = match (Const(1), ConstP(1)) = SOME []

val test11c = match (Const(1), Wildcard) = SOME []
val test11d = match (Const(1), Variable("s")) = SOME [("s",Const(1))]
val test11e = match (Tuple [Const(1)], TupleP [Variable("s")]) = SOME [("s",Const(1))]
val test11f = match (Tuple [Const(1),Const(2)], TupleP [Variable("s")]) = NONE
val test11g = match (Tuple [Const(1),Const(2)], TupleP [Variable("s"),Wildcard]) = SOME [("s",Const(1))]
val test11h = match (Tuple [Const(1),Const(2),Constructor("x",Const(3))], TupleP [Variable("s"),Wildcard,ConstructorP("x",Variable "v")]) = SOME [("s",Const(1)),("v",Const(3))] orelse match (Tuple [Const(1),Const(2),Constructor("x",Const(3))], TupleP [Variable("s"),Wildcard,ConstructorP("x",Variable "v")]) = SOME [("v",Const(3)),("s",Const(1))]

val test11i = match (Tuple [Const(1),Const(2),Constructor("x",Const(3))], TupleP [Variable("s"),Wildcard,ConstructorP("y",Variable "v")]) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test12a = first_match Unit [] = NONE
val test12b = first_match (Const(1)) [UnitP,Variable("x")] = SOME [("x",Const(1))]
