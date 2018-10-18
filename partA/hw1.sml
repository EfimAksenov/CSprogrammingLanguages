fun is_older(date1: (int*int*int), date2: (int*int*int)) =
    if #1 date1 <> #1 date2
    then #1 date1 < #1 date2
    else if #2 date1 <> #2 date2
    then #2 date1 < #2 date2
    else if #3 date1 <> #3 date2
    then #3 date1 < #3 date2
    else false

fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) = 
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

fun date_to_string(date: int*int*int) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, numbers: int list) =
    let
        fun helper(result: int, index: int) =
            if (result + List.nth(numbers, index)) < sum
            then helper(result + List.nth(numbers, index), index + 1)
            else index
    in 
       helper(0, 0) 
    end

fun what_month(dayOfYear: int) =
    let
        val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(dayOfYear, months) + 1
    end

fun month_range(day1: int, day2: int) = 
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(allDates: (int*int*int) list) =
    if null allDates
    then NONE
    else let
        fun find_oldest(dates: (int*int*int) list, oldestDate: int*int*int) =
            if null dates
            then oldestDate
            else if is_older(oldestDate, hd dates)
            then find_oldest(tl dates, oldestDate)
            else find_oldest(tl dates, hd dates)
    in
        SOME (find_oldest(tl allDates, hd allDates))
    end

fun removeDoubleInts(loi: int list) =
    let 
        fun contains(loi: int list, num: int) =
            if null loi
            then false
            else if hd loi = num
            then true
            else contains(tl loi, num)
    in
            if null loi
            then []
            else if contains(tl loi, hd loi)
            then removeDoubleInts(tl loi)
            else (hd loi)::removeDoubleInts(tl loi)
    end 

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
    number_in_months(dates, removeDoubleInts months)

fun reasonable_date(date: int*int*int) = 
    let
        val months = [31,28,31,30,31,30,31,31,30,31,30,31]
        val monthsLeap = [31,29,31,30,31,30,31,31,30,31,30,31]
        fun isMonthInRange(month: int) =
            month > 0 andalso month <= 12
        fun isDayInRange(day: int, month: int, leap: bool) =
            let val daysNum = if leap then List.nth(monthsLeap, month - 1) else List.nth(months, month - 1) in
                day > 0 andalso day <= daysNum
            end
        fun isLeapYear(year: int) =
            if year mod 400 = 0
            then true
            else (year mod 4 = 0) andalso (year mod 100 <> 0)
    in
        (#1 date > 0) andalso isMonthInRange(#2 date) andalso isDayInRange(#3 date, #2 date, isLeapYear(#1 date))
    end
