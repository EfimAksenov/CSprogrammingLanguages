(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun swapArgs f y x = f x y
fun curry f x y = f (x,y)

val only_capitals = List.filter (Char.isUpper o swapArgs (curry String.sub) 0)

val longest_string1 = foldl (fn (xn,acc) => if (String.size xn) > (String.size acc) then xn else acc) ""

val longest_string2 = foldl (fn (xn,acc) => if (String.size xn) >= (String.size acc) then xn else acc) ""

fun longest_string_helper f = foldl (fn (xn,acc) => if f ((String.size xn),(String.size acc)) then xn else acc) ""

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f alist =
    case alist of
        [] => raise NoAnswer
    |   x::xs => case f x of
                    NONE => (first_answer f xs)
                |   SOME v => v

fun all_answers f alist = 
    let
        fun aux(acc, alist) =
            case alist of
                [] => SOME acc
            |   x::xs => case f x of
                            NONE => NONE
                        |   SOME v => aux(v @ acc, xs)
    in
        aux([], alist)
    end

val count_wildcards = g (fn () => 1) (fn (s) => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size s)

fun count_some_var (sToMatch,p) = g (fn () => 0) (fn s => if s = sToMatch then 1 else 0) p

val check_pat =
    let
        fun getListOfVariables p = 
            case p of
	          Wildcard          => []
	        | Variable x        => [x]
	        | TupleP ps         => List.foldl (fn (p,acc) => getListOfVariables p @ acc) [] ps
	        | ConstructorP(_,p) => getListOfVariables p
	        | _                 => []
        fun hasNoDuplicates los = 
            case los of
                [] => true
            |   s::[] => true
            |   x::xs => not (List.exists (fn s => x= s) xs) andalso hasNoDuplicates xs
    in
        hasNoDuplicates o getListOfVariables
    end

fun match (v,p) =
    case (p,v) of
        (Wildcard,_) => SOME []
    |   (Variable s,_) => SOME [(s,v)]
    |   (UnitP,Unit) => SOME []
	|   (ConstP x,Const y) => if x = y then SOME [] else NONE
	|   (TupleP lop, Tuple lov) => ((all_answers match (ListPair.zipEq (lov,lop))) handle 
                                                        ListPair.UnequalLengths => NONE)
	|   (ConstructorP (s1,p),Constructor (s2,v)) => if s1 = s2 then match (v,p) else NONE
    |   (_,_) => NONE

fun first_match v lop =
    (SOME (first_answer (fn p => match(v,p)) lop)) handle NoAnswer => NONE
