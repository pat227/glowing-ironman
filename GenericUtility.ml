module GenericUtility (*: sig
  val print_GMTime : unit -> unit
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val thd : 'a * 'b * 'c -> 'c
  val print2Afile_Int32Map : int32 MAP.t  -> outfile:string -> unit
end*) = struct
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
  (*Needs double checking; unsure twitter abbreviates all these exactly this way*)
  let month2int s =
      match s with
	  "Jan" -> 0
	| "Feb" -> 1
	| "Mar" -> 2
	| "Apr" -> 3
	| "May" -> 4
	| "June" -> 5
	| "July" -> 6
	| "Aug" -> 7
	| "Sept" -> 8
	| "Oct" -> 9
	| "Nov" -> 10
	| "Dec" -> 11
	  (*this should never happen unless I've bungled an abbreviation*)
	| _ -> -1;;

  let fst (x, _) = x;;
  let snd (_, x) = x;;
  let fst3 (x, _, _) = x;;
  let snd3 (_, x, _) = x;;
  let thd3 (_, _, x) = x;;

  (*This nested module must be used by outside module to take advantage of the print function*)
  module Int32_INT_MAP = Map.Make(Int32);;

  let print2Afile_Int32Map ~amap ~outfile =
    let outchan = open_out outfile in
    let bindings = Int32_INT_MAP.bindings amap in
    let rec helper ~list ~outchan = 
      match list with 
	  i2i_binding :: rest -> let key = fst(i2i_binding) in
				 let value = snd(i2i_binding) in
				 begin
				   output_string outchan (Int32.to_string key);
				   output_string outchan ", ";
				   output_string outchan (Int32.to_string value);
				   output_char outchan '\n';
				   helper ~list:rest ~outchan:outchan;
				 end;
	| [] -> (close_out outchan; (););
    in
    helper ~list:bindings ~outchan:outchan;;
  
end
