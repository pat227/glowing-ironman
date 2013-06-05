(*Used during eyeball inspection for patters in retweets and how to approach the problem more generally; 
  grep proved difficult or unable to identify only those tweets without nested json for retweets*)
open GenericUtility;;
module SeperateTWEETS = struct
  let isRT ~line =
    let regexp_retweet = Str.regexp "retweeted_status" in
    try Str.search_forward regexp_retweet line 0 with Not_found -> -1;;
  
  let seperateFile ~file ~outfileA ~outfileB =
    let inchan = open_in file in
    let outchanA = open_out outfileA in
    let outchanB = open_out outfileB in 
    let rec helper ~outchanA ~outchanB ~inchan =
      let nextline = try (Some (input_line inchan)) with End_of_file -> (close_in inchan; 
									 close_out outchanA; 
									 close_out outchanB;
									 None;) in
      match nextline with
	  Some l -> if (isRT ~line:l) >= 0 then 
	      begin
		output_string outchanA l;
		output_string outchanA "\n";
		helper ~outchanA:outchanA ~outchanB:outchanB ~inchan:inchan;
	      end
	    else 
	      begin
		output_string outchanB l;
		output_string outchanB "\n";
		helper ~outchanA:outchanA ~outchanB:outchanB ~inchan:inchan;
	      end
	| None -> ();
    in
    helper ~outchanA:outchanA ~outchanB:outchanB ~inchan:inchan;;
  
  let main () =
    let arg1 = Sys.argv.(1) in
    let arg2 = Sys.argv.(2) in
    let arg3 = Sys.argv.(3) in
    begin
      print_string "\nStarted at: ";
      GenericUtility.print_GMTime ();
      print_newline ();
      seperateFile ~file:arg1 ~outfileA:arg2 ~outfileB:arg3;
      print_string "\nFinished at: ";
      GenericUtility.print_GMTime ();
      print_newline();
      exit 0;
    end;;
end
