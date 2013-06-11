open GenericUtility;;
(*This short module needed to provide an ordered type so we can make sets and maps in next module;
  also defines a few useful operations on the records.*)
module TweetRecord : sig 
  type t = { _id_ : int;            (*unique tweet id as before*) 
	     _userid_: int ;        (*New datum: user id of the tweeter*)
	     _username_: string ;   (*New datum: username of the tweeter*)
	     _time_string_: string; (*ripped from twitter json, just as before*)
	     _text_: string;        (*New datum: tweet text in full*)
	     _adhocRTuserID_ : int; (*New datum: unique userID of "RT @ username", if present, else zero*)
	     _adhocRTusername_ : string };; (*String form RT @ username - more easily recognizable*)
  val compare: t -> t -> int
  val print_FatTweetRecord : trec:t -> unit
  val printHTable : hashtbl:(t, t) Hashtbl.t -> outfile:string -> unit
  val tm_fromString : string -> Unix.tm
  val emptyTrec : unit -> t
  val printlist : t list -> unit
end = struct
  type t = { _id_ : int; 
	     _userid_: int ; 
	     _username_: string ; 
	     _time_string_: string; 
	     _text_: string;
	     _adhocRTuserID_ : int;
	     _adhocRTusername_ : string };;

  (*val compare: t -> t -> int *)
  let compare t1 t2 = Int64.compare (Int64.of_int t1._id_) (Int64.of_int t2._id_);;

  (*Print to screen -- had an unused named argument, and compiler didn't complain!?
  val print_FatTweetRecord : trec:t -> unit*)
  let print_FatTweetRecord ~trec = 
    begin
      print_string " tweetid: ";
      print_string (string_of_int trec._id_);
      print_string ", ";
      print_string " userid: ";
      print_string (string_of_int trec._userid_);
      print_string ", ";
      print_string " username: ";
      print_string trec._username_;
      print_string ", ";
      print_string " text: ";
      print_string trec._text_;
      print_string " ";
      print_string " time: ";
      print_string trec._time_string_;
      print_string " ";
      if trec._adhocRTuserID_ > 0 then
	begin
	  print_string " ";
	  print_string (string_of_int trec._adhocRTuserID_);
	  print_string " ";
	  print_string trec._adhocRTusername_;
	  print_newline ();
	end
      else print_newline ();
    end;;

  (*Print to file a hashtable of [new fat] tweetrecords
    val printHTable : hashtbl:(t, t) Hashtbl.t -> outfile:string -> unit*)
  let printHTable ~hashtbl ~outfile = 
    let outchan = open_out outfile in 
    (*Makes note of id serving as key, and doesn't print except once while 
      printing the set to which it is mapped.*)
    let priorid = ref 0 in
    let writeFold outchan treckey trecord = 
      let id = treckey._id_ in
      if id != !priorid then
	begin 
	  priorid := id;
	  output_string outchan (string_of_int id);
	  output_string outchan ", ";
	  output_string outchan (string_of_int treckey._userid_);
	  output_string outchan ", ";
	  output_string outchan treckey._username_;
	  output_string outchan ", ";
	  output_string outchan treckey._text_;
	  output_string outchan ", ";
	  output_string outchan treckey._time_string_;
	  if treckey._adhocRTuserID_ > 0 then
	    begin
	      output_string outchan " ";
	      output_string outchan (string_of_int treckey._adhocRTuserID_);
	      output_string outchan " ";
	      output_string outchan treckey._adhocRTusername_;
	    end
	  else ();
	  output_string outchan " -> ";
	  output_string outchan (string_of_int trecord._id_);
	  output_string outchan ", ";
	  output_string outchan (string_of_int trecord._userid_);
	  output_string outchan ", ";
	  output_string outchan trecord._username_;
	  output_string outchan ", ";
	  output_string outchan trecord._text_;
	  output_string outchan ", ";
	  output_string outchan trecord._time_string_;
	  if trecord._adhocRTuserID_ > 0 then
	    begin
	      output_string outchan " ";
	      output_string outchan (string_of_int trecord._adhocRTuserID_);
	      output_string outchan ", ";
	      output_string outchan trecord._adhocRTusername_;
	    end
	  else ();
	  output_string outchan "\n";
	end
      else
	begin (*42 spaces @ present to accomodate source-tweet id, date*)
	  output_string outchan "                                          | ";
	  output_string outchan (string_of_int trecord._id_);
	  output_string outchan ", ";
	  output_string outchan (string_of_int trecord._userid_);
	  output_string outchan ", ";
	  output_string outchan trecord._username_;
	  output_string outchan ", ";
	  output_string outchan trecord._text_;
	  output_string outchan ", ";
	  output_string outchan trecord._time_string_;
	  if trecord._adhocRTuserID_ > 0 then
	    begin
	      output_string outchan " ";
	      output_string outchan (string_of_int trecord._adhocRTuserID_);
	      output_string outchan ", ";
	      output_string outchan trecord._adhocRTusername_;
	    end
	  else ();
	  output_string outchan "\n";
	end
    in
    begin
      Hashtbl.iter (writeFold outchan) hashtbl;
      close_out outchan;
    end;;

  (*Convert the heretofore string ripped from the json into a unix tm struct.
    Could be made more robust although the json data as input seems to be very
    uniform at hasn't yet caused any errors in here.
    val tm_ofTweet : string -> Unix.tm *)
  let tm_fromString timestring = 
    (*of the form: "Mar 11 12:39:05 2013"*)
    let space1 = (String.index_from timestring 0 ' ') + 1 in
    let space2 = (String.index_from timestring space1 ' ') + 1 in
    let space3 = (String.index_from timestring space2 ' ') + 1 in
    let length = String.length timestring in
    let m = String.trim (String.sub timestring 0 (space1-1)) in
    let month = GenericUtility.month2int m in
    let d = String.trim (String.sub timestring space1 (space2-space1)) in
    let day = int_of_string d in
    let y = String.trim (String.sub timestring space3 (length-space3)) in
    let year = int_of_string y in
    let hours = int_of_string (String.sub timestring space2 2) in
    let minutes = int_of_string (String.sub timestring (space2+3) 2) in
    let seconds = int_of_string (String.sub timestring (space2+6) 2) in
    { Unix.tm_sec = seconds; tm_min = minutes; tm_hour = hours; 
      tm_mday = day; tm_mon = month; tm_year = (year-1900); tm_wday = 0; 
      tm_yday = 0; tm_isdst = false};;

  let emptyTrec () = { _id_ =0 ; 
	     _userid_= 0 ; 
	     _username_ = "" ; 
	     _time_string_ =  ""; 
	     _text_ = "";
	     _adhocRTuserID_ = 0 ;
	     _adhocRTusername_ = "" };;

  (* Print elements of a list of tweetrecords.
     t list -> unit
  *)
  let rec printlist elems = 
    match elems with 
      h :: t -> (print_FatTweetRecord ~trec:h;
		 printlist t;)
    | [] -> ();;

end
