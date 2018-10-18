(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true
val test11 = is_older ((1,2,3),(1,2,3)) = false
val test12 = is_older ((1,2,3),(1,2,2)) = false

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test21 = number_in_month ([(2012,2,28),(2013,12,1)],3) = 0
val test22 = number_in_month ([],3) = 0

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test31 = number_in_months ([],[2,3,4]) = 0
val test32 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test33 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5,6,7]) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test41 = dates_in_month ([],2) = []
val test42 = dates_in_month ([(2012,2,28),(2013,12,1)],3) = []

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test51 = dates_in_months ([],[2,3,4]) = []
val test52 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test53 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5,6,7]) = []

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test61 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test62 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test71 = date_to_string (2013, 1, 1) = "January 1, 2013"
val test72 = date_to_string (2013, 12, 1) = "December 1, 2013"

val test8 = number_before_reaching_sum (1, [1,2,3,4,5]) = 0
val test81 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test82 = number_before_reaching_sum (2, [1,2,3,4,5]) = 1

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test121 = removeDoubleInts([1,2,3,4]) = [1,2,3,4]
val test122 = removeDoubleInts([1,2,3,3]) = [1,2,3]
val test123 = removeDoubleInts([]) = []
val test124 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,4]) = 3
val test125 = number_in_months_challenge ([],[2,3,4,4]) = 0
val test126 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test127 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5,6,7]) = 0

val test13 = reasonable_date (0, 2, 15) = false
val test131 = reasonable_date (2018, 1, 15) = false
val test133 = reasonable_date (2018, 11, 31) = true
val test134 = reasonable_date (2020, 2, 29) = true
val test135 = reasonable_date (2021, 2, 29) = false
