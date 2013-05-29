module GenericUtility : sig
  val print_GMTime : unit -> unit
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val thd : 'a * 'b * 'c -> 'c
end = struct
  (*For anyone who does not have time output as part of 
    the prompt with which to better guage runtimes*)
  let print_GMTime () = 
    let tmstruct = Unix.gmtime(Unix.time ()) in
    let int2month i =
      match i with
	  0 -> "Jan"
	| 1 -> "Feb"
	| 2 -> "Mar"
	| 3 -> "Apr"
	| 4 -> "May"
	| 5 -> "June"
	| 6 -> "July"
	| 7 -> "Aug"
	| 8 -> "Sept"
	| 9 -> "Oct"
	| 10 -> "Nov"
	| 11 -> "Dec"
	| _ -> "NOT A MONTH"
    in
    let int2wday i =
      match i with
	  0 -> "Sun"
	| 1 -> "Mon"
	| 2 -> "Tue"
	| 3 -> "Wed"
	| 4 -> "Thur"
	| 5 -> "Fri"
	| 6 -> "Sat"
	| _ -> "NOT A DAY"
    in
    begin
      print_string "[";
      if tmstruct.Unix.tm_hour < 10 then (print_int 0; 
					  print_int tmstruct.Unix.tm_hour;) else print_int tmstruct.Unix.tm_hour;
      print_string ":";
      if tmstruct.Unix.tm_min < 10 then (print_int 0;
					 print_int tmstruct.Unix.tm_min;) else print_int tmstruct.Unix.tm_min;
      print_string ":";
      if tmstruct.Unix.tm_sec < 10 then (print_int 0;
					 print_int tmstruct.Unix.tm_sec;) else print_int tmstruct.Unix.tm_sec;
      print_string " | ";
      print_string (int2wday (tmstruct.Unix.tm_wday));
      print_string " ";
      print_string (int2month (tmstruct.Unix.tm_mon));
      print_string " ";
      print_int (tmstruct.Unix.tm_mday);
      print_string "]";
    end;;

  let fst (x, _) = x;;
  let snd (_, x) = x;;
  let thd (_, _, x) = x;;
  
end
