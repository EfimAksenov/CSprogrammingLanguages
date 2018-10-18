(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []
val test111 = all_except_option ("string", ["string", "NONE"]) = SOME ["NONE"]
val test112 = all_except_option ("NODE", ["string", "NONE"]) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test211 = get_substitutions1 ([["foo", "bar"],["there"]], "foo") = ["bar"]
val test212 = get_substitutions1 ([["foo", "bar"],["there", "foo"]], "foo") = ["bar", "there"] 

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test311 = get_substitutions2 ([["foo", "bar"],["there"]], "foo") = ["bar"]
val test312 = get_substitutions2 ([["foo", "bar"],["there", "foo"]], "foo") = ["bar", "there"] 

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black
val test51 = card_color (Diamonds, Num 2) = Red

val test6 = card_value (Clubs, Num 2) = 2
val test61 = card_value (Clubs, Ace) = 11
val test62 = card_value (Clubs, King) = 10

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test71 = remove_card ([(Hearts, Ace), (Hearts, King)], (Hearts, Ace), IllegalMove) = [(Hearts, King)]
val test72 = remove_card ([(Hearts, Ace), (Hearts, King), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, King), (Hearts, Ace)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test81 = all_same_color [(Hearts, Ace)] = true
val test82 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Ace)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test91 = sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13
val test92 = sum_cards [(Clubs, Num 2),(Clubs, Ace),(Clubs, King)] = 23
val test93 = sum_cards [(Clubs, Num 2),(Clubs, Ace),(Clubs, King),(Clubs, Num 6)] = 29

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test101 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test102 = score ([(Hearts, Num 2),(Clubs, Num 4),(Clubs, Ace)],15) = 6
val test103 = score ([(Clubs, Num 2),(Clubs, Num 4),(Clubs, Ace)],15) = 3
val test104 = score ([(Clubs, Num 4),(Clubs, Ace)],15) = 0

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test1111 = officiate ([(Hearts, Ace),(Clubs, Ace),(Clubs, King)],
                          [Draw,Draw,Draw], 15) = 21
val test1112 = officiate ([(Hearts, Ace),(Diamonds, Ace),(Clubs, King)],
                          [Draw,Draw,Draw], 15) = 10 

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42) = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],[Draw,Discard(Hearts,Jack)],42); false) handle IllegalMove => true)
