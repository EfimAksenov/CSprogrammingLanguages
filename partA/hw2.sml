(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, los) =
    let
       fun has_string los =
        case los of
            [] => false
        |   x::xs => if same_string(str, x) then true else has_string xs
        
        fun exclude los =
        case los of
            [] => []
        |   x::xs => if same_string(str, x) then exclude xs else x::exclude xs
    in
       if has_string los then SOME (exclude los) else NONE 
    end

fun get_substitutions1 (lolos, str) =
    case lolos of
        [] => []
    |   x::xs => case all_except_option(str, x) of
                    NONE => get_substitutions1(xs, str)
                |   SOME los => los @ get_substitutions1(xs, str)

fun get_substitutions2 (lolos, str) =
    let
        fun aux(lolos, result) =
            case lolos of
                [] => result
            |   x::xs => case all_except_option(str, x) of
                            NONE => aux(xs, result)
                        |   SOME los => aux(xs, result @ los)
    in
        aux(lolos, [])
    end

fun similar_names(lolos: string list list, {first=first, middle=middle, last=last}) =
    let
        fun generate(los) =
            case los of 
                [] => []
            |   x::xs => {first=x,middle=middle,last=last}::generate(xs)
    in
       generate(first::get_substitutions2(lolos, first))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(suit, rank)  =
    case suit of
        Clubs => Black
    |   Spades => Black
    |   Diamonds => Red
    |   Hearts => Red

fun card_value(suit, rank) =
    case rank of
        Ace => 11
    |   King => 10
    |   Queen => 10
    |   Jack => 10
    |   Num n => n

fun remove_card(loc, c, exp) =
    case loc of
        [] => raise exp
    |   x::xs => if x=c then xs else x::remove_card(xs,c,exp)

fun all_same_color(loc): bool =
    case loc of
        [] => true
    |   x::[] => true
    |   x::y::xs => (case (card_color x, card_color y) of
                        (Red, Red) => true
                    |   (Black, Black) => true
                    |   (_, _) => false)
                    andalso (all_same_color(y::xs))
fun sum_cards loc =
    let
        fun aux(loc, acc) =
            case loc of
                [] => acc
            |   x::xs => aux(xs, acc + card_value x)
    in
        aux(loc, 0)
    end

fun score(loc, goal) =
    let
        val cardSum = sum_cards loc
        val ps = if cardSum < goal then goal - cardSum else (cardSum - goal) * 3
    in
        if all_same_color loc then ps div 2 else ps
    end

fun officiate(loc, lom, goal) =
    let
        fun aux(loc, lom, acc) =
            case lom of
                [] => acc
            |   x::xs => case (x,loc) of
                            (Discard c,_) => aux(loc, xs, remove_card(acc, c, IllegalMove))
                        |   (Draw,y::ys) => if sum_cards(y::acc) > goal
                                            then (y::acc)
                                            else aux(ys, xs, (y::acc))
                        |   (_,_) => acc
    in
        score(aux(loc, lom, []), goal)
    end

(* put your solutions for problem 2 here *)
