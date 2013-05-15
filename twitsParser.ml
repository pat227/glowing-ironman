(*Runs so far on smallest data set (286MB compressed, 2.4 GB decompressed):
  PYHTON: 12 minutes
  OCAML:
     with ~8MB read chunksize bytecode -> 32 minutes
     with ~16MB read chunksize native code -> 2 hours, 43 minutes  (memory consumption never broke 15%, possibly even 12%)
     improved code with ~8MB read chunksize native code -> 3 minutes 34 seconds!!! Beats Python handily!
     (Initial results show 800MB (100 8MB chunks) processed in a little under 90 seconds while printing to screen too)

     The improved code avoids what was heavy use of the substring operation which involves lots of copying of memory; also the 8MB chunksize might be working to my advantage due to the 8MB L2 cache size on my local machine...just guess.

  Mar12-13.bz2 (2.5GB compressed) runtimes
  PYTHON: ~48 hours
  OCAML: improved native code with ~8MB chunksize -> ~56 minutes HUGE IMPROVEMENT
  Started (Function time) [18:58:14 | Wed May 15]  (Command line time) [14:54:28|Wed May 15]
  Memory usage hit ~35% @ ~18 minutes into run; ~42.7% @ ~22 minutes into run; ~53% @ ~29 minutes; ~81% @ ~43 minutes;
  FINISHED (Function time) [19:48:45 | Wed May 15]  (Command line time) [15:48:46|Wed May 15]
*)

module GenericUtility : sig
  val print_GMTime : unit -> unit
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
end


(*Tweet-specific functions go here*)
module ParseBZ2Tweets = struct

  (*We might want to store the time not as a string in future; & we might want
    to add the text of the tweet itself at some point*)
  type tweet_record = {id : int; time_string: string;};;

  let print_tweetrecord ~trec = 
    begin
      print_string " {";
      print_string (string_of_int trec.id);
      print_string ", ";
      print_string trec.time_string;
      print_string "} ";
      print_newline ();
    end;;
    
  (*Based on the mar9-11 dataset, the python script output was 4,692,976 lines 
    long (898M), an unknown number of which were not keys; also that run took 
    almost 48 hours due to extreme memory pressure--lets hope that doesn't 
    repeat itself. *)
  let tweets_HASHtbl : (tweet_record, tweet_record) Hashtbl.t = Hashtbl.create(4194304);;

  (*Pretty prints hashtable to file*)
  (*val printHTable : hashtbl:(int, tweet_record) Hashtbl.t -> outfile:string -> unit *)
  let printHTable ~hashtbl ~outfile = 
    let outchan = open_out outfile in 
    (*Makes note of id serving as key, and doesn't print except once while 
      printing the set to which it is mapped.*)
    let priorid = ref 0 in
    let writeFold outchan treckey trecord = 
      let id = treckey.id in
      if id != !priorid then
	begin 
	  priorid := id;
	  output_string outchan (string_of_int id);
	  output_string outchan ", ";
	  output_string outchan treckey.time_string;
	  output_string outchan " -> ";
	  output_string outchan (string_of_int trecord.id);
	  output_string outchan ", ";
	  output_string outchan trecord.time_string;
	  output_string outchan "\n";
	end
      else
	begin (*42 spaces @ present to accomodate source-tweet id, date*)
	  output_string outchan "                                          | ";
	  output_string outchan (string_of_int trecord.id);
	  output_string outchan ", ";
	  output_string outchan trecord.time_string;
	  output_string outchan "\n";
	end
    in
    begin
      Hashtbl.iter (writeFold outchan) hashtbl;
      close_out outchan;
    end;;

  (*Be sure to #require "bz2"
    val openBZ2file : string -> Bz2.in_channel*)
  let openBZ2file ~file =
    let ch = Bz2.open_in (open_in file) in
    ch;;

  (*Function to expand a buffer before getting it's string representation; 
    conveneint way of getting strings of arbitrary length, needed by bz2
    functions that modify strings for generting ouput so long as they are
    long enough.*)
  (*val fillBuffer : buffer:Buffer.t -> numchars:int -> string = <fun>*)
  let fillBuffer ~buffer ~numchars = 
    (*Use ! b/c the overwritten string buffer should not have any "!" 
      chars in it and these are relatively easy to spot*)
    (*Bug fix: starting from zero causes off by one error; json string
      fail to parse b/c at boundaries of reads from archive a "!" char
      would be present within random position of json string*)
    for i = 1 to numchars do
      Buffer.add_char buffer '!';
    done;
    Buffer.contents buffer;;

  (*Clearer intention that just hinging on true/false or other values*)
  type inputstate = 
      Good
    | Bad
    | EndofFile;;

  (*Use return type to avoid potential and imagined incomplete matches*)
  type parse_result = Parsed of Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json
		      | NoParse;;
  
  (*val input_next_x_chars : Bz2.in_channel -> int -> string*)
  let input_next_x_chars ~bzinchan ~howmanymore =
    let buf = Buffer.create howmanymore in
    let sbuf = fillBuffer ~buffer:buf ~numchars:howmanymore in
    try 
      (*print_string "===Getting more chars from bz2 archive...===";
      print_newline ();*)
      ignore (Bz2.read bzinchan sbuf 0 howmanymore);
      (*print_string "===Got: "; print_string sbuf;
      print_newline ();*)
      (Good, sbuf);
    with End_of_file -> (Bz2.close_in bzinchan;
			 (EndofFile, sbuf;))
      | _ -> (Bz2.close_in bzinchan;
			 (Bad, sbuf;));;

  let input_next_x_chars_improved ~bzinchan ~howmanymore =
    let buf = Buffer.create howmanymore in
    let sbuf = fillBuffer ~buffer:buf ~numchars:howmanymore in
    try 
      (*print_string "===Getting more chars from bz2 archive...===";
	print_newline ();*)
      ignore (Bz2.read bzinchan sbuf 0 howmanymore);
      (*print_string "===Got: "; print_string sbuf;
      print_newline ();*)
      (Good, sbuf);
    with End_of_file -> (Bz2.close_in bzinchan;
			 (EndofFile, sbuf;))
      | _ -> (Bz2.close_in bzinchan;
	      (Bad, sbuf;));;  

  (*So long as we have an incomplete tweet...keep getting more from the bz2 file; each is seperated by newline*)
  let rec get_next_tweet ~bzinchan ~chunksize ~sbufold =
    if String.contains sbufold '\n' then
      let pos = String.index sbufold '\n' in 
      let length = String.length sbufold in
      (Good, (String.sub sbufold 0 pos), (String.sub sbufold (pos+1) (length - pos - 1)))
    else
      begin
	let t = input_next_x_chars ~bzinchan:bzinchan ~howmanymore:chunksize in 
	match t with
	    (Good, sbuf2) -> 
	      let merged = sbufold ^ sbuf2 in
	      if String.contains sbuf2 '\n' then
		let length = String.length merged in
		let pos = String.index merged '\n' in	
		(Good, (String.sub merged 0 pos), (String.sub merged (pos+1) (length - pos - 1)))
	      else 
		get_next_tweet ~bzinchan:bzinchan ~chunksize:chunksize ~sbufold:merged
	  | (_, sbuf2) ->
	    let merged = sbufold ^ sbuf2 in
	    if String.contains sbuf2 '\n' then
	      let length = String.length merged in
	      let pos = String.index merged '\n' in	
	      (Bad, (String.sub merged 0 pos), (String.sub merged (pos+1) (length - pos - 1)))
	    else
	      (Bad, merged, "")
      end;;

			  
  (*Extract unique id, time stamp for inclusion in a dictionary*)
  (*val get_id_time_retweets : line:string ->
      Yojson.Basic.json option * Yojson.Basic.json option *
      Yojson.Basic.json option
  *)
  let get_id_time_retweets ~line = 
    let j = try      
	      (*print_string "===Parsing line: "; print_string line; print_newline ()*)
	      Yojson.Safe.from_string line;
      with _ -> (print_string "===Failed to Parse JSON: "; print_string line; print_newline (); `Null)
    in
    (*Protect against tweets that r not retweets--ignore those*)
    let retweet_status = try 
			   Yojson.Basic.Util.member "retweeted_status" (Yojson.Safe.to_basic j) 
      with _ -> ((*print_string "===Failed to Parse B=== "; print_newline ();*) `Null)
    in
    match retweet_status with
	`Null ->  ((*print_string "===Failed to parse; returning None triple..."; print_newline ();*) NoParse);
      | _ -> (let id = Yojson.Basic.Util.member "id" (Yojson.Safe.to_basic j) in
	      let rid = Yojson.Basic.Util.member "id" retweet_status in
	      let time = Yojson.Basic.Util.member "created_at" (Yojson.Safe.to_basic j) in
	      let rtime = Yojson.Basic.Util.member "created_at" retweet_status in
	      (*print_string "===Parsed; returning triple..."; 
	      print_string (Yojson.Basic.to_string id);
	      print_string (Yojson.Basic.to_string rid);
	      print_string (Yojson.Basic.to_string rtime);
	      print_newline ();*)
	      (Parsed (id, time, rid, rtime)));;

(*  ====SLOWER OLD VERSION THAT DID TOO MUCH MEMORY COPYING====
  (*Given bz2 tweet archive file, retrieves tweets one at a time and *)
  (*val process_file : file:string -> outfile:string -> unit*)
  let process_file ~file ~outfile ~htbl = 
    let bz2inchan = openBZ2file ~file:file in
    let createTrecord ~id ~time =
      let id_s = Yojson.Basic.to_string id in
      let id_i = int_of_string id_s in
      let time_s_proto = Yojson.Basic.to_string time in
      (*"Mon Mar 11 12:39:05 +0000 2013" wastes space, cut it down to: "Mar 11 12:39:05 2013"*)
      let time_s = (String.sub time_s_proto 5 15) ^ (String.sub time_s_proto 26 5) in
      {id = id_i; time_string = time_s}
    in
    let rec helper ~bz2inchan ~outfile ~htbl ~buf = 
      (*The chunksize hugely affects runtime performance; obviously we'll never suffer huge memory
	pressure as under python's bz2 but it appears we also run a little slower in the case of
	small archives while running much better in the case of large archives*)
      let nexttweet = get_next_tweet ~bzinchan:bz2inchan ~chunksize:8388608 ~sbufold:buf in
      match nexttweet with
	  (Good, toconsume, leftovers) ->
	    (let tuple = get_id_time_retweets ~line:toconsume in
	     match tuple with
		 NoParse -> helper ~bz2inchan:bz2inchan ~outfile:outfile ~htbl:htbl ~buf:leftovers
	       | (Parsed (id, time, rid, rtime)) -> 
		 let sourcetweet = createTrecord ~id:rid ~time:rtime in
		 let retweet = createTrecord ~id:id ~time:time in
		 begin
		   (*print_string "===Adding record for:";
		     print_string id_s;
		     print_tweetrecord ~trec:newtweetrec;*)
		   Hashtbl.add htbl sourcetweet retweet;
		   helper ~bz2inchan:bz2inchan ~outfile:outfile ~htbl:htbl ~buf:leftovers;
		 end;)
	| (_, toconsume, leftovers) -> 
	  (let tuple = get_id_time_retweets ~line:toconsume in
	   match tuple with
	       NoParse -> 
		 begin
		   print_string "===Printing Hashtable and cleaning up.";
		   printHTable ~hashtbl:htbl ~outfile:outfile;
		   ();
		 end
	     | (Parsed (id, time, rid, rtime)) -> 
	       let sourcetweet = createTrecord ~id:rid ~time:rtime in
	       let retweet = createTrecord ~id:id ~time:time in
	       begin
		 (*print_string "===Adding record for:";
		   print_string id_s;
		   print_tweetrecord ~trec:newtweetrec;*)
		 Hashtbl.add htbl sourcetweet retweet;
		 print_string "===Printing Hashtable and cleaning up.";
		 printHTable ~hashtbl:htbl ~outfile:outfile;
		 ();
	       end)
    in
    helper ~bz2inchan:bz2inchan ~outfile:outfile ~htbl:htbl ~buf:"";;
*)

  (*The chunksize hugely affects runtime performance; obviously we'll never suffer huge memory
    pressure as under python's bz2 but it appears we also run a little slower in the case of
    small archives while running much better in the case of large archives*)
  (*Given bz2 tweet archive file, retrieves tweets one at a time and *)
  (*val process_file : file:string -> outfile:string -> unit*)
  let process_file_improved ~file ~outfile ~htbl ~chunksize = 
    let bz2inchan = openBZ2file ~file:file in
    let fst (x, _) = x in
    let snd (_, x) = x in
    let createTrecord ~id ~time =
      let id_s = Yojson.Basic.to_string id in
      let id_i = int_of_string id_s in
      let time_s_proto = Yojson.Basic.to_string time in
      (*"Mon Mar 11 12:39:05 +0000 2013" wastes space, cut it down to: "Mar 11 12:39:05 2013"*)
      let time_s = (String.sub time_s_proto 5 15) ^ (String.sub time_s_proto 26 5) in
      {id = id_i; time_string = time_s}
    in
    let rec consumeBuf ~htbl ~stringbuf ~startpos =
      let nextbreak = try 
			String.index_from stringbuf startpos '\n' 
	with _ -> (print_string "===Consumed 8MB buffer with a prior startpos of: ";
		   print_string (string_of_int startpos);
		   print_newline ();
		   String.length stringbuf) in
      if nextbreak == (String.length stringbuf) then 
	(htbl, startpos) 
      else
	let toconsume = String.sub stringbuf startpos (nextbreak - startpos) in
	let tuple = get_id_time_retweets ~line:toconsume in
	match tuple with
	    (Parsed (id, time, rid, rtime)) -> 
	      let sourcetweet = createTrecord ~id:rid ~time:rtime in
	      let retweet = createTrecord ~id:id ~time:time in
	      begin
		(*print_string "===Adding record for:";
		  print_string sourcetweet.id;
		  print_tweetrecord ~trec:retweet;*)
		Hashtbl.add htbl sourcetweet retweet;
		consumeBuf ~htbl:htbl ~stringbuf:stringbuf ~startpos:(nextbreak+1);
	      end;
	  | NoParse -> 
	    consumeBuf ~htbl:htbl ~stringbuf:stringbuf ~startpos:(nextbreak+1)
    in		 
    let rec helper ~bz2inchan ~outfile ~htbl ~chunksize ~leftover = 
      (*begin
	print_string "===Leftover: ";
	print_string leftover;*)
      let nextchunk = input_next_x_chars_improved ~bzinchan:bz2inchan ~howmanymore:chunksize in
      match nextchunk with
	  (Good, toconsume) ->  
	    let merged = leftover ^ toconsume in 
	    let t = consumeBuf ~htbl:htbl ~stringbuf:merged ~startpos:0 in
	    let updatedhtbl = fst t in
	    let leftoverstart = snd t in
	    let leftover = String.sub merged leftoverstart ((String.length merged) - leftoverstart) in	    
	    helper ~bz2inchan:bz2inchan ~outfile:outfile ~htbl:updatedhtbl ~chunksize:chunksize ~leftover:leftover;
	| (_, toconsume) -> 
	  let t = consumeBuf ~htbl:htbl ~stringbuf:toconsume ~startpos:0 in
	  let updatedhtbl = fst t in
	  begin
	    print_string "===Printing Hashtable and cleaning up.";
	    printHTable ~hashtbl:updatedhtbl ~outfile:outfile;
	    ();
	  end
	(*end*)
    in
    helper ~bz2inchan:bz2inchan ~outfile:outfile ~htbl:htbl ~chunksize:chunksize ~leftover:"";;
  

  (*val main : unit -> 'a*)
  let main () =
    let arg1 = Sys.argv.(1) in   (* eg "mar11.bz2"*) 
    let arg2 = Sys.argv.(2) in   (* eg "ocamlout.txt"*)
    begin
      print_string "\nStarted at: ";
      GenericUtility.print_GMTime ();
      print_newline ();
      process_file_improved ~file:arg1 ~outfile:arg2 ~htbl:tweets_HASHtbl ~chunksize:8388608;
      (*process_file ~file:arg1 ~outfile:arg2 ~htbl:tweets_HASHtbl;*)
      print_string "\nFinished at: ";
      GenericUtility.print_GMTime ();
      print_newline();
      exit 0;
    end;;
  main ();;
end
  
  


(*==============FAILS===================
  USING A STATIC reference buffer was ambitious and somehow didn't work; upon re-entry the
  contents of the buffer are lost, but are not lost when it recursively calls itself, but
  that's enough to screw us up.

  (*Does what it says; maintains internal static buffer while consuming it
    or fetching or more text from compressed archive until at least 1 
    complete tweet can be returned to caller.*)
  let rec get_next_tweet ~bzinchan ~chunksize =
    let eof = ref false in
    let staticbuff = ref (Buffer.create chunksize) in
    let fillBuffer ~buffer ~numchars = 
      for i = 0 to numchars do
	Buffer.add_char buffer '0';
      done;
      Buffer.contents buffer in
    (*val input_next_x_chars : Bz2.in_channel -> int -> string*)
    let input_next_x_chars ~bzinchan ~howmanymore ~eofflag:flag =
      let buf = Buffer.create howmanymore in
      let sbuf = fillBuffer ~buffer:buf ~numchars:howmanymore in
      try 
	print_string "===Getting more chars from bz2 archive...===";
	print_newline ();
	ignore (Bz2.read bzinchan sbuf 0 howmanymore);
	print_string "===Got: "; print_string sbuf;
	print_newline ();
	sbuf;
      with End_of_file -> (Bz2.close_in bzinchan;
			   flag := true;
			   sbuf;)
	| _ -> (Bz2.close_in bzinchan;
		flag := true;
		sbuf;)
    in
    fun () -> 
      begin (*Ugly way of indicating EOF, but is first buffer producer/consumer 
	      implementation I've ever written for ocaml*)
	print_string "===Static Buffer initial state:";
	print_string (Buffer.contents !staticbuff);
	print_newline ();
	if !eof then "ENDOFFILE" else
	  let rwedoneold = String.contains (Buffer.contents !staticbuff) '\n' in
	  (*if buffer already has 1 complete tweet, just consume it from buffer*)
	  if rwedoneold then
	    let oldpos = String.index (Buffer.contents !staticbuff) '\n' in
	    let oldlen = String.length (Buffer.contents !staticbuff) in
	    let buf2 = Buffer.create (String.length (Buffer.contents !staticbuff)) in
	    begin
	      print_string "===Buffer already had a tweet in it; consuming some of it...===";
	      print_newline ();
	      Buffer.add_string buf2 (Buffer.contents !staticbuff);
	      Buffer.clear !staticbuff;
	      Buffer.add_string !staticbuff (String.sub (Buffer.contents buf2) (oldpos + 1) (oldlen - oldpos - 1));
	      String.sub (Buffer.contents buf2) 0 oldpos;
	    end
	  else  (*buffer doesn't have even 1 complete tweet, get more decomressed text from dataset*)
	    let rwedonenew = ref false in
	    let updatedBuff_s = "" in
	    while not !rwedonenew && not !eof do
	      let nextblock = input_next_x_chars ~bzinchan:bzinchan ~howmanymore:chunksize ~eofflag:eof in
	      let _ = Buffer.add_string !staticbuff nextblock in
	      (*updatedBuff_s = Buffer.contents !staticbuff;*)
	      print_string "===Static buffer updated...=== ";
	      (*print_string (Buffer.contents !staticbuff);*)
	      print_newline ();
	      rwedonenew := String.contains (Buffer.contents !staticbuff) '\n';
	    done;
	    let length = String.length (Buffer.contents !staticbuff) in
	    let pos = String.index (Buffer.contents !staticbuff) '\n' in
	    let buf2 = Buffer.create (String.length (Buffer.contents !staticbuff)) in
	    begin
	      print_string "===Buffer did not have tweet in it, had to go get more text...===";
	      print_string "===Buffer has: ";
	      print_string (Buffer.contents !staticbuff);
	      print_string "===Found newline at pos:"; print_string (string_of_int pos);
	      print_newline ();
	      Buffer.add_string buf2 (Buffer.contents !staticbuff);
	      Buffer.clear !staticbuff;
	      Buffer.add_string !staticbuff (String.sub (Buffer.contents buf2) (pos+1) (length - pos - 1));
	      String.sub (Buffer.contents buf2) 0 pos;
	    end;
      end;;
*)



