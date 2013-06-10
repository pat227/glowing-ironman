(*This short module needed to provide an ordered type so we can make sets and maps in next module;
  also defines a few useful operations on the records.*)
open GenericUtility;;
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
	      output_string outchan " ";
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
	      output_string outchan " ";
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


(*Reconstruct some of the hash table created from first pass from a file--should have a manner of doing so 
  live as well--and then fill in tweet text, user ids and names, etc, and try to find flows via adhoc retweets 
  by examining the entire dataset again; this cannot be done for text b/c memory pressure even for a relatively
  "small" data set is enormous even with more fine grained memory management. *)
module Followup (*: sig 
  val main : unit -> unit (*The only exposed function*)
end*) = struct
  let popular_enough = 51;;
  (*Set of old tweets, lacking text, etc*)
  module Tweetset = Set.Make(TweetRecord);;
  (*Set of tweets with text, etc, aka "fat" records*)
  module FatSet = Set.Make(TweetRecord);;
  (*Set of adhoc tweets with text, etc, aka fat adhoc records*)
  module AdHocFatSet = Set.Make(TweetRecord);;
  (*A good amount of this module and the one above share some functions that will be factored out into
    a common dependency to avoid duplication, bloat, etc.*)

  let tweets_HASHtbl_OLD : (TweetRecord.t, TweetRecord.t) Hashtbl.t = Hashtbl.create(8192);;
  (*Took a short while to get the next line exactly correct*)
  let tweets_HASHtbl_NEW : (TweetRecord.t, TweetRecord.t) Hashtbl.t = Hashtbl.create(8192);;

  (*Simple function to get around isomorphic type constraints that in our case hinders work. 
    val convert : t1:Tweetset.elt -> TweetRecord.t = <fun>*)
  let convertTSelementt2TR ~(t1:Tweetset.elt) : TweetRecord.t = 
    { TweetRecord._id_ = t1.TweetRecord._id_;
      TweetRecord._time_string_ = t1.TweetRecord._time_string_ ;
      TweetRecord._userid_ = t1.TweetRecord._userid_ ;
      TweetRecord._username_ = t1.TweetRecord._username_ ;
      TweetRecord._text_ = t1.TweetRecord._text_ ;
      TweetRecord._adhocRTuserID_ = t1.TweetRecord._adhocRTuserID_;
      TweetRecord._adhocRTusername_ = t1.TweetRecord._adhocRTusername_ };;

  (*Clearer intention that just hinging on true/false or other values*)
  type inputstate = 
      Good
    | Bad
    | EndofFile;;

  (*Use return type to avoid potential or illusory incomplete matches*)
  type parse_result = 
    RetweetParsed of 
	Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * 
	  Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * 
	  Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * 
	  Yojson.Basic.json
  | AdHocRetweetParsed of 
      Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json *
	Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json *
	Yojson.Basic.json
  | NoParse;;

  (*val input_next_x_chars_improved :
      bzinchan:Bz2.in_channel -> howmanymore:int -> inputstate * string*)
  let input_next_x_chars_improved ~bzinchan ~howmanymore =
    (*let buf = Buffer.create howmanymore in*)
    let sbuf = String.make howmanymore '!' in (*fillBuffer ~buffer:buf ~numchars:howmanymore in*)
    try 
      ignore (Bz2.read bzinchan sbuf 0 howmanymore);
      (Good, sbuf);
    with End_of_file -> (Bz2.close_in bzinchan;
			 (EndofFile, sbuf;))
      | _ -> (Bz2.close_in bzinchan;
	      (Bad, sbuf;));;  

  (*So long as we have an incomplete tweet...keep getting more from the bz2 file; each is seperated by newline
    val get_next_tweet :
      bzinchan:Bz2.in_channel ->
      chunksize:int -> sbufold:string -> inputstate * string * string
  *)
  let rec get_next_tweet ~bzinchan ~chunksize ~sbufold =
    if String.contains sbufold '\n' then
      let pos = String.index sbufold '\n' in 
      let length = String.length sbufold in
      (Good, (String.sub sbufold 0 pos), (String.sub sbufold (pos+1) (length - pos - 1)))
    else
      begin
	let t = input_next_x_chars_improved ~bzinchan:bzinchan ~howmanymore:chunksize in 
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

  (*Gets id, userid, username, time & text of a tweet & retweet pair (not adhoc retweets)
    val getRetweetsExtraInfo : line:string -> parse_result   *)
  let getRetweetsExtraInfo ~line = 
    let j = try      
	      (*print_string "===Parsing line: "; print_string line; print_newline ()*)
	      Yojson.Safe.from_string line;
      with _ -> (print_string "===Failed to Parse JSON: "; print_string line; print_newline (); `Null)
    in
    (*Protect against tweets that are not retweets--ignore those*)
    let retweet_status = try 
			   Yojson.Basic.Util.member "retweeted_status" (Yojson.Safe.to_basic j) 
      with _ -> ((*print_string "===Failed to Parse B=== "; print_newline ();*) `Null)
    in
    match retweet_status with
	`Null ->  ((*print_string "===Failed to parse; returning None..."; print_newline ();*) NoParse);
      | _ -> (let jbasic = Yojson.Safe.to_basic j in
	      let id = Yojson.Basic.Util.member "id" jbasic in
	      let rid = Yojson.Basic.Util.member "id" retweet_status in
	      let time = Yojson.Basic.Util.member "created_at" jbasic in
	      let rtime = Yojson.Basic.Util.member "created_at" retweet_status in
	      let userj = Yojson.Basic.Util.member "user" jbasic in 
	      let userid = Yojson.Basic.Util.member "id" userj in 
	      let username = Yojson.Basic.Util.member "screen_name" userj in
	      let text = Yojson.Basic.Util.member "text" jbasic in 
	      let ruserj = Yojson.Basic.Util.member "user" retweet_status in
	      let ruserid = Yojson.Basic.Util.member "id" ruserj in
	      let rusername = Yojson.Basic.Util.member "screen_name" ruserj in
	      let rtext = Yojson.Basic.Util.member "text" retweet_status in
	      (RetweetParsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)))
  
  (*From file with entries that look like:    "310250954186444801, Mar 09 04:49:26 2013".
    Had a pair of off by 1 errors that cost many hours to find.
    val trecord_from_string : trecstring:string -> TweetRecord.t
  *)
  let trecord_from_string ~trecstring = 
    let pos = String.index_from trecstring 0 ',' in
    let id = String.sub trecstring 0 pos in
    let id_int = int_of_string id in
    let time = String.sub trecstring (pos + 2) ((String.length trecstring) - pos - 2) in
    { TweetRecord._id_ = id_int; 
      _time_string_ = time; 
      _userid_ = 0 ; 
      _username_ = ""; 
      _text_ = ""; 
      _adhocRTuserID_ = 0;
      _adhocRTusername_ = ""; };; 
  
  (*(Parsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)))
    val createTrecord :
    id:Yojson.Basic.json ->
    time:Yojson.Basic.json ->
    userid:Yojson.Basic.json ->
    username:Yojson.Basic.json -> 
    ?adhocuserid:Yojson.Basic.json ->
    ?adhocusername:Yojson.Basic.json ->
    text:Yojson.Basic.json -> TweetRecord.t
  *)
  let createTrecord ~id ~time ~userid ~username ?adhocuserid ?adhocusername ~text =
    let id_s = Yojson.Basic.to_string id in
    let id_i = int_of_string id_s in
    let time_s_proto = Yojson.Basic.to_string time in
    (*"Mon Mar 11 12:39:05 +0000 2013" wastes space, cut it down to: "Mar 11 12:39:05 2013"*)
    let time = (String.sub time_s_proto 5 15) ^ (String.sub time_s_proto 26 5) in
    let userid_s = Yojson.Basic.to_string userid in
    let userid = int_of_string userid_s in
    let username = Yojson.Basic.to_string username in
    let text = Yojson.Basic.to_string text in
    match adhocuserid, adhocusername with
      None, None -> let userid_adhoc = 0 in 
		    let username_adhoc = "" in
		    { TweetRecord._id_ = id_i; 
		      _time_string_ = time; 
		      _userid_ = userid ; 
		      _username_ = username; 
		      _text_ = text;
		      _adhocRTuserID_ = userid_adhoc;
		      _adhocRTusername_ = username_adhoc }
    | Some x, Some y -> let userid_adhoc = int_of_string (Yojson.Basic.to_string x) in
			let username_adhoc = Yojson.Basic.to_string y in
			{ TweetRecord._id_ = id_i; 
			  _time_string_ = time; 
			  _userid_ = userid ; 
			  _username_ = username; 
			  _text_ = text;
			  _adhocRTuserID_ = userid_adhoc;
			  _adhocRTusername_ = username_adhoc }
    | None, Some y -> let userid_adhoc = 0 in
		      let username_adhoc = Yojson.Basic.to_string y in
		      { TweetRecord._id_ = id_i; 
			_time_string_ = time; 
			_userid_ = userid ; 
			_username_ = username; 
			_text_ = text;
			_adhocRTuserID_ = userid_adhoc;
			_adhocRTusername_ = username_adhoc }
    | Some x, None -> let userid_adhoc = int_of_string (Yojson.Basic.to_string x) in
		      let username_adhoc = "" in 
		      { TweetRecord._id_ = id_i; 
			_time_string_ = time; 
			_userid_ = userid ; 
			_username_ = username; 
			_text_ = text;
			_adhocRTuserID_ = userid_adhoc;
			_adhocRTusername_ = username_adhoc };;

  (*Add a list to a table, where head of list is the key, and rest are mappings from key
    val addToTable_Set :
    list:Tweetset.elt list ->
    tbl:(Tweetset.elt, Tweetset.elt) Hashtbl.t ->
    set:Tweetset.t -> (Tweetset.elt, Tweetset.elt) Hashtbl.t * Tweetset.t
  *)
  let addToTable_Set ~list:accum ~tbl:htbl ~set:set = 
    let key = List.nth accum 0 in
    let rec helper ~set ~key ~elemlist ~htbl =
      match elemlist with
	h :: tail -> if h == key then 
	    let updatedset = Tweetset.add h set in
	    helper ~key:key ~set:updatedset ~elemlist:tail ~htbl:htbl 
	  else
	    begin 
	      Hashtbl.add htbl key h;
	      let updatedset = Tweetset.add h set in
	      helper ~key:key ~set:updatedset ~elemlist:tail ~htbl:htbl
	    end
      | [] -> (htbl, set) in
    helper ~set:set ~htbl:htbl ~elemlist:accum ~key:key;;

  module FreqMap = GenericUtility.Int32_INT_MAP;;
    
  (*Parse the file that was a hashtable of keys->mappings; find those tweets with > X retweets mappings; 
    make a set of those only while also reconstructing the original hashtable. Also create histogram-ready
    frequency distribution map.
    val parseTable4PopularTweets :
    file:string -> (Tweetset.elt, Tweetset.elt) Hashtbl.t * Tweetset.t * int32 FreqMap.t
  *)
  let parseTable4PopularTweets ~file =
    let inchan = open_in file in
    let regexp_arrow = Str.regexp " -> " in
    let rec helper ~inchan ~accum ~htbl ~set ~map =
      let nextline = try (Some (input_line inchan)) with End_of_file -> (close_in inchan; None) in
      match nextline with 
	  None -> (htbl, set, map) (*if nextline in file is new key mapping, add to table prior key/elements 
		       mapping if exceeds cutoff*)
	| Some line -> 
	  (let hasarrow = try if Str.search_forward regexp_arrow line 0 > 0 then true else false with _ -> false in
	   match hasarrow with true ->
	     let listlength = List.length accum in
	     let int32listlength = Int32.of_int listlength in
	     let updatedMap = try (*BUGFIX: I had this next line before the match; this is proper place*)
	       FreqMap.add int32listlength (Int32.add (Int32.of_int 1) (FreqMap.find int32listlength map)) map 
	       with Not_found -> FreqMap.add int32listlength (Int32.of_int 1) map in
	     if listlength > popular_enough then
	       let tbl_set = addToTable_Set ~list:accum ~tbl:htbl ~set:set in
	       let updatedTable = GenericUtility.fst(tbl_set) in
	       let updatedSet = GenericUtility.snd(tbl_set) in
	       let pos = Str.search_forward regexp_arrow line 0 in
	       let firstHalf = String.sub line 0 pos in
	       let key = trecord_from_string ~trecstring:firstHalf in
	       let secondHalf = String.sub line (pos+4) ((String.length line) - pos - 4) in
	       let firstelement = trecord_from_string ~trecstring:secondHalf in
	       let updatedaccum = key :: firstelement :: [] in
	       helper ~inchan:inchan ~accum:updatedaccum ~htbl:updatedTable ~set:updatedSet ~map:updatedMap
	     else 
	       let pos = Str.search_forward regexp_arrow line 0 in
	       let firstHalf = String.sub line 0 pos in
	       let key = trecord_from_string ~trecstring:firstHalf in
	       let secondHalf = String.sub line (pos+4) ((String.length line) - pos - 4) in
	       let firstelement = trecord_from_string ~trecstring:secondHalf in
	       let updatedaccum = key :: firstelement :: [] in
	       helper ~inchan:inchan ~accum:updatedaccum ~htbl:htbl ~set:set ~map:updatedMap
	   (*if nextline in file is another mapping for same prior key, grow list of mappings*)
	   | false ->
	     (let pos = String.index line '|' in
	      let onlypart = String.sub line (pos + 2) ((String.length line) - pos - 2) in
	      let nextelement = trecord_from_string ~trecstring:onlypart in
	      let updatedaccum = accum @ [nextelement] in
	      helper ~inchan:inchan ~accum:updatedaccum ~htbl:htbl ~set:set ~map:map))
    in
    helper ~inchan:inchan ~accum:[] ~htbl:tweets_HASHtbl_OLD ~set:Tweetset.empty ~map:FreqMap.empty;;
  
  (*val openBZ2file : file:string -> Bz2.in_channel*)
  let openBZ2file ~file =
    let ch = Bz2.open_in (open_in file) in
    ch;;

  (*Took a little while to figure out that the RHS also needed the module name
    val truncatedTweet : tweet:TweetRecord.t -> TweetRecord.t
  *)
  let truncatedTweet ~(tweet:TweetRecord.t) : TweetRecord.t = 
    { TweetRecord._id_ = tweet.TweetRecord._id_; 
      _time_string_ = tweet.TweetRecord._time_string_; 
      _userid_ = 0 ; 
      _username_ = ""; 
      _text_ = "";
      _adhocRTuserID_ = 0;
      _adhocRTusername_ = "";};;    

  (*Better idea: using old thinset Htbl and new fatset, print the old thinset augmented by the datums
    in the fatset rather than trying to reconstruct a fat version of the htbl; and write to disk as file. 
    Later on we could use the output file as input to create a clean copy of the fat htbl. Also we'll 
    benefit from the decreasing size of the set as we pull elements out to print them.
    val constructNewHtblOnDisk : set:Tweetset.t -> 
    oldHtbl:(TweetRecord.t, TweetRecord.t) Hashtbl.t -> 
    outfile:string -> unit
  *)
  let constructNewHtblOnDisk ~outfile ~fatset ~thinhtbl =
    let outchan = open_out outfile in
    let priorid = ref 0 in
    (*let setref = ref fatset in*)
    let semiequal tweetA tweetB = 
      if tweetA.TweetRecord._id_ == tweetB.TweetRecord._id_
      then true else false in
    let writeFold outchan fatset treckey trecord = 
      let id = treckey.TweetRecord._id_ in
      let fatkey = Tweetset.choose (Tweetset.filter (semiequal treckey) fatset) in
      let fatrecord = Tweetset.choose (Tweetset.filter (semiequal trecord) fatset) in
      if id != !priorid then
	begin 
	  priorid := id;
	  output_string outchan (string_of_int id);
	  output_string outchan ", ";
	  output_string outchan (string_of_int fatkey.TweetRecord._userid_);
	  output_string outchan ", ";
	  output_string outchan fatkey.TweetRecord._username_;
	  output_string outchan ", ";
	  output_string outchan fatkey.TweetRecord._text_;
	  output_string outchan ", ";
	  output_string outchan fatkey.TweetRecord._time_string_;
	  if fatkey.TweetRecord._adhocRTuserID_ > 0 then
	    begin
	      output_string outchan " ";
	      output_string outchan (string_of_int fatkey.TweetRecord._adhocRTuserID_);
	      output_string outchan " ";
	      output_string outchan fatkey.TweetRecord._adhocRTusername_;
	    end
	  else ();
	  output_string outchan " -> ";
	  output_string outchan (string_of_int fatrecord.TweetRecord._id_);
	  output_string outchan ", ";
	  output_string outchan (string_of_int fatrecord.TweetRecord._userid_);
	  output_string outchan ", ";
	  output_string outchan fatrecord.TweetRecord._username_;
	  output_string outchan ", ";
	  output_string outchan fatrecord.TweetRecord._text_;
	  output_string outchan ", ";
	  output_string outchan fatrecord.TweetRecord._time_string_;
	  if fatrecord.TweetRecord._adhocRTuserID_ > 0 then
	    begin
	      output_string outchan " ";
	      output_string outchan (string_of_int fatrecord.TweetRecord._adhocRTuserID_);
	      output_string outchan " ";
	      output_string outchan fatrecord.TweetRecord._adhocRTusername_;
	    end
	  else ();
	  output_string outchan "\n";
	end
      else
	begin (*42 spaces @ present to accomodate source-tweet id, date*)
	  output_string outchan "                                          | ";
	  output_string outchan (string_of_int fatrecord.TweetRecord._id_);
	  output_string outchan ", ";
	  output_string outchan (string_of_int fatrecord.TweetRecord._userid_);
	  output_string outchan ", ";
	  output_string outchan fatrecord.TweetRecord._username_;
	  output_string outchan ", ";
	  output_string outchan fatrecord.TweetRecord._text_;
	  output_string outchan ", ";
	  output_string outchan fatrecord.TweetRecord._time_string_;
	  if trecord.TweetRecord._adhocRTuserID_ > 0 then
	    begin
	      output_string outchan (string_of_int fatrecord.TweetRecord._adhocRTuserID_);
	      output_string outchan " ";
	      output_string outchan fatrecord.TweetRecord._adhocRTusername_;
	    end
	  else ();
	  output_string outchan "\n";
	end
    in
    begin
      print_string "===Starting to write a fat htbl to disk===";
      print_newline ();
      Hashtbl.iter (writeFold outchan fatset) thinhtbl;
      close_out outchan;
      print_string "===Finished writing fat htbl to disk===";
      print_newline ();
    end;;
      

  (*Determine more quickly if a line of text has a nested element indicative of being a non-adhoc retweet
    val isRT : line:string -> int*)
  let isRT ~line =
    let regexp_retweet = Str.regexp "retweeted_status" in
    try Str.search_forward regexp_retweet line 0 with Not_found -> -1;;
  
  (*This function likely has a better more direct alternative
    val getUID_fromAdhocRT_mention :
    tweetjson:Yojson.Basic.json -> Yojson.Basic.json * Yojson.Basic.json
  *)
  let getUID_fromAdhocRT_mention ~tweetjson = 
    (*Get several entities mentioned in tweet, including users, hashtags, urls*)
    let entities = Yojson.Basic.Util.filter_member "entities" [tweetjson] in
    (*Get list of assoc lists for each user entity mentioned in tweet*)
    let user_mentions = Yojson.Basic.Util.filter_member "user_mentions" entities in
    (*Get only assoc list within list for our one user from RT @ username*)
    let unwrapped = Yojson.Basic.Util.filter_list user_mentions in
    (*get the only entry in the assoc list*)
    let unwrapped2 = List.nth unwrapped 0 in
    (*Extract the only list in that assoc list*)
    let unwrapped3 = Yojson.Basic.Util.filter_assoc unwrapped2 in
    (*Get 5 tuples of the assoc list: keys are: screen_name, name, id, id_str, indices*)
    let unwrapped4 = List.nth unwrapped3 0 in 
    (*Get 3rd element of list, the userid as int*)
    let useridjson = List.nth unwrapped4 2 in 
    (*Convert from json to int*)
    let userid_adhoc = (*Yojson.Basic.Util.to_int*) (snd useridjson) in
    (*also get the RT @ username *)
    let username_adhoc = (*Yojson.Basic.Util.to_int*) (snd (List.nth unwrapped4 0)) in
    (if userid_adhoc == `Null || username_adhoc == `Null then print_string "===Error starts here===" else ();
    (userid_adhoc, username_adhoc));;

  (*===BETTER COMBINED FUNCTION VERSION=== 
    So we can do 1 pass over bz2 file, build our fatset and collect adhoc retweets in same pass.
    val getALLRetweets : line:string -> parse_result
  *)
  let getALLRetweets ~line = 
    let j = try      
	      (*print_string "===Parsing line: "; print_string line; print_newline ()*)
	      Yojson.Basic.from_string line;
      with _ -> (print_string "===Failed to Parse JSON: "; print_string line; print_newline (); `Null)
    in
    let retweet_status = try 
			   Yojson.Basic.Util.member "retweeted_status" j
      with _ -> ((*print_string "===Failed to Parse B=== "; print_newline ();*) `Null)
    in
    match retweet_status with
      `Null ->  (let text = 
		   try 
		     Yojson.Basic.Util.member "text" j 
		   with _ -> (print_string "Error line 681"; print_newline (); `Null) in 
		 let textstring = 
		   try (*a json line of this form is randomly crashing me here: { "limit": { "track": 27189957 } }*)
		     Yojson.Basic.Util.to_string text 
		   with _ -> ((*print_string "Error line 685"; 
			      print_newline ();
			      print_string " the json: "; 
			      print_string (Yojson.Basic.pretty_to_string j); 
			      print_newline ();*) " ") in
		 (*Other possible regexp:  (RT.?\u[a-z0-9]*.?@) or better  (RT[^@]{0,5}@)  but {} not recognized by Str
		   or "RT[^@]@" also works*)
		 let regexp_retweet = Str.regexp "\\(\\(RT[^@]@\\)\\|\\(RT[^@][^@]@\\)\\)" in
		 let pos = try Str.search_forward regexp_retweet textstring 0 with _ -> -1 in
		 let pos2 = try Str.search_forward regexp_retweet textstring (pos+1) with _ -> -1 in
		 match pos, pos2 with
		   -1, -1 -> NoParse (*not an adhoc retweet*)
		 | -1,  pos2 when pos2 != -1 -> NoParse (*impossible*)
		 |  pos1, pos2 when pos1 != -1 && pos2 != -1 -> NoParse (*too many RT @ usernames*)
		 |  pos1, -1 when pos1 != -1 -> (*possible adhoc retweet we're looking for*)
		   (*make a record out it; store it; later we'll try to find where it might fit*)
		   (try let after_rt_at_tuple = getUID_fromAdhocRT_mention ~tweetjson:j in
		    let uid_after_rt_at = GenericUtility.fst after_rt_at_tuple in
		    let uname_after_rt_at = GenericUtility.snd after_rt_at_tuple in
		    let id = Yojson.Basic.Util.member "id" j in
		    let time = Yojson.Basic.Util.member "created_at" j in
		    let userj = Yojson.Basic.Util.member "user" j in 
		    let userid = Yojson.Basic.Util.member "id" userj in 
		    let username = Yojson.Basic.Util.member "screen_name" userj in
		    let text = Yojson.Basic.Util.member "text" j in
		    AdHocRetweetParsed (id, time, userid, username, text, uid_after_rt_at, uname_after_rt_at)
		    with _ -> (print_string "Error line 634; found an adhoc tweet whose json lacked user_mentions despite RT @ username."; 
			       print_newline (); 
			       NoParse;
		    (*print_string (Yojson.Basic.pretty_to_string j); 
		      print_newline (); 
		      getUID_name_only ~tweetjson:j*)
		    ))
		 | _, _ -> NoParse)
    | _ -> (try 
	      let id = Yojson.Basic.Util.member "id" j in
	      let rid = Yojson.Basic.Util.member "id" retweet_status in
	      let time = Yojson.Basic.Util.member "created_at" j in
	      let rtime = Yojson.Basic.Util.member "created_at" retweet_status in
	      let userj = Yojson.Basic.Util.member "user" j in 
	      let userid = Yojson.Basic.Util.member "id" userj in 
	      let username = Yojson.Basic.Util.member "screen_name" userj in
	      let text = Yojson.Basic.Util.member "text" j in 
	      let ruserj = Yojson.Basic.Util.member "user" retweet_status in
	      let ruserid = Yojson.Basic.Util.member "id" ruserj in
	      let rusername = Yojson.Basic.Util.member "screen_name" ruserj in
	      let rtext = Yojson.Basic.Util.member "text" retweet_status in
	      RetweetParsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)
	with _ -> (print_string "Error line 725"; print_newline (); NoParse));;

  let updateThinSet ~set:set ~tweet:tweet = 
    let predicate ~id tweet =
      if tweet.TweetRecord._id_ == id then true else false in
    let p = predicate ~id:tweet.TweetRecord._id_ in
    let one_elementset = FatSet.filter p set in
    let truncatedt = truncatedTweet ~tweet:tweet in
    let oldcount = FatSet.cardinal set in
    begin
      if FatSet.cardinal one_elementset == 1 then 
	let newset = FatSet.remove truncatedt set in
	let count = FatSet.cardinal newset in
	if count != (oldcount-1) then
	  (print_string "Error; line 818; set not reducing by one!";
	   exit 2;)
	else ();	  
	FatSet.add tweet newset;
      else
	set
    end;;
  
  (* val fillFatSet_And_AdhHocTweetSet :
     adhocset:AdHocFatSet.t ->
     fatset:Tweetset.t ->
     compressedfile:string -> Tweetset.t * AdHocFatSet.t*)
  let fillFatSet_And_AdhHocTweetSet ~adhocset ~thinset ~compressedfile =
    let bz2inchan = openBZ2file ~file:compressedfile in
    let rec consumeBuf ~adhocset ~thinset ~stringbuf ~startpos =
      let nextbreak = try 
			String.index_from stringbuf startpos '\n' 
	with _ -> (print_string "===Consumed 8MB buffer with a prior startpos of: ";
		   print_string (string_of_int startpos);
		   print_newline ();
		   String.length stringbuf) in
      if nextbreak == (String.length stringbuf) then 
	(thinset, startpos, adhocset)
      else
	let toconsume = String.sub stringbuf startpos (nextbreak - startpos) in
	let tuple = try 
		      getALLRetweets ~line:toconsume 
	  with _ -> (print_string "Error line 695"; print_newline (); exit 2) in
	match tuple with 
	  (RetweetParsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)) -> 
	    let sourcetweet = try
	       createTrecord ~id:rid ~time:rtime ~userid:ruserid 
	      ~username:rusername ~text:rtext ?adhocuserid:None ?adhocusername:None 
	      with _ -> (print_string "Error line 757"; print_newline (); exit 2)
	    in
	    let retweet = 
	      createTrecord ~id:id ~time:time ~userid:userid 
		~username:username ~text:text ?adhocuserid:None ?adhocusername:None in
	    let updatedSet = updateThinSet ~set:thinset ~tweet:sourcetweet in
	    let updatedSet2 = updateThinSet ~set:updatedSet ~tweet:retweet in
	    begin
	      (*print_string "Sourcetweet:";
	      TweetRecord.print_FatTweetRecord sourcetweet;
	      print_string "Retweet:";
	      TweetRecord.print_FatTweetRecord retweet;*)
	      consumeBuf 
		~stringbuf:stringbuf ~startpos:(nextbreak+1) 
		~thinset:updatedSet2 ~adhocset:adhocset
	    end
	| (AdHocRetweetParsed (id, time, userid, username, text, uid_after_rt_at, uname_after_rt_at)) -> 
	    let adhocrt = 
	      createTrecord ~id:id ~time:time ~userid:userid 
	      ~username:username ~adhocuserid:uid_after_rt_at 
	      ~adhocusername:uname_after_rt_at ~text:text in
	    let updatedAdHocSet = AdHocFatSet.add adhocrt adhocset in
	    consumeBuf 
	      ~stringbuf:stringbuf ~startpos:(nextbreak+1) 
	      ~thinset:thinset ~adhocset:updatedAdHocSet
	| NoParse -> 
	  consumeBuf 
	    ~thinset:thinset ~stringbuf:stringbuf 
	    ~startpos:(nextbreak+1) ~adhocset:adhocset
    in
    let rec helper ~thinset ~adhocset ~bz2inchan ~chunksize ~leftover = 
      let nextchunk = input_next_x_chars_improved ~bzinchan:bz2inchan ~howmanymore:chunksize in
      match nextchunk with
	(Good, toconsume) ->  
	  let merged = leftover ^ toconsume in 
	  let t = consumeBuf ~adhocset:adhocset ~thinset:thinset ~stringbuf:merged ~startpos:0 in
	  let updatedset = GenericUtility.fst3 t in
	  let leftoverstart = GenericUtility.snd3 t in
	  let updatedAdHocSet = GenericUtility.thd3 t in
	  let leftover = String.sub merged leftoverstart ((String.length merged) - leftoverstart) in	    
	  helper ~thinset:updatedset ~adhocset:updatedAdHocSet ~bz2inchan:bz2inchan 
	    ~chunksize:chunksize ~leftover:leftover;
      | (_, toconsume) -> 
	let t = consumeBuf ~thinset:thinset ~adhocset:adhocset ~stringbuf:toconsume ~startpos:0 in
	let updatedFatSet = GenericUtility.fst3 t in
	let updatedAdHocSet = GenericUtility.thd3 t in
	begin
	  print_string "===Finished creating popular tweets set & adhoc set===";
	  (updatedFatSet, updatedAdHocSet);
	end
    in
    helper ~thinset:thinset ~adhocset:adhocset ~bz2inchan:bz2inchan ~chunksize:8388608 ~leftover:"";;

  (*Invoke functions that parse the tweet -> retweet mapping file; finds
    popular tweets; for each popular tweet retrieves from bz2 archive additional
    datums such as text, userid, username; creates a new hashtable and 
    ("fat")set with these additional datums for these popular tweets; also
    identifies adhoc retweets from bz2 archive in same pass and makes a set of
    these including full text, tweetid, userid, username, and RT@username & id.
    val submain : unit -> unit * FatSet.t * AdHocFatSet.t
  *)  
  let submain ~infile ~outfile ~outfile2 ~oldhtblfile =
    let triple = parseTable4PopularTweets ~file:oldhtblfile in
    let oldhtbl = GenericUtility.fst3 triple in
    let thinset = GenericUtility.snd3 triple in
    let freqmap = GenericUtility.thd3 triple in
    let tuple = 
      fillFatSet_And_AdhHocTweetSet 
	~adhocset:AdHocFatSet.empty 
	~thinset: thinset
	~compressedfile:infile in
    let fattweetset = GenericUtility.fst tuple in
    let adhocset = GenericUtility.snd tuple in
    (*Bug in constructNewHtbl and we don't really need it
    let newHtbl = constructNewHtbl ~set:fattweetset ~oldHtbl:oldhtbl 
      ~newHtbl:tweets_HASHtbl_NEW in*)
    begin
      (*TweetRecord.printlist (Tweetset.elements fattweetset); *)
      constructNewHtblOnDisk ~outfile:outfile2 ~fatset:fattweetset ~thinhtbl:oldhtbl;
      print_string "===No. of Popular tweets & retweets found: ";
      print_string (string_of_int (FatSet.cardinal fattweetset));
      print_newline ();
      print_string "===No. of AdHoc tweets (with only 1 RT@username) found: ";
      print_string (string_of_int (AdHocFatSet.cardinal adhocset));
      print_newline ();
      GenericUtility.print2Afile_Int32Map ~amap:freqmap ~outfile:outfile;
      (*TweetRecord.printHTable ~hashtbl:newHtbl ~outfile:"fathtbl.txt";*)
      (fattweetset, adhocset);
    end;; 
  
  (*Given a fatset of popular known tweets and retweets, and a set of adhoc retweets, 
    create a hashtable mapping any known tweet or retweet or any adhoc retweet,
    to any adhoc retweet. Match up RT @ "username" and userID and check that
    timestamps do not preclude a possible flow before adding such a mapping.
    Later on eliminate mappings that fail some edit distance or greatest common
    substring threshold. Also eyeball these mappings.
    The complexity could be too large: should be A^2 + AB where A is # of elements
    in the set of adhoc retweets and B is the # of elements in the set of known
    tweets and retweets. Another way of considering the complexity is (XN)^2 where
    X is some ration between 0 and 1 and N is the total number of elements in 
    both sets (the adhoc and known tweet and retweet sets). The ratio X is unknown
    because I don't know how many adhoc retweets exist, but it should be substantial
    compared to the number of popular tweets and retweets.
    This is better however than doing an all pairs edit distance metric which would
    be N^2 over all tweets, ie, equivalent to this function with an impossible ratio
    for X of 1, plus the added complexity of every single edit distance computation. 
    So in that sense actually this function has much lower complexity in the sense 
    that we dispense with computation of any edit distance or common substring computation
    alltogether, reserving that for later.
    Note that elements in the set of adhoc retweets are not removed when added to
    the mappings because none of the mappings are certain. Perhaps in future with a
    good text metric we could reduce the set size as we add mappings, reducing complexity
    and run time.
    val constructCandidateFlows :
    fatset:FatSet.t ->
    adhocset:AdHocFatSet.t -> (FatSet.elt, FatSet.elt) Hashtbl.t
  *)
  let constructCandidateFlows ~fatset ~adhocset = 
    let listA = FatSet.elements fatset in
    let listB = AdHocFatSet.elements adhocset in
    let candidates = Hashtbl.create(32768) in
    let isCandidate ~tweetA ~tweetB =
      (*Note that ALL tweets have a userid, and only adhoc retweets have non-zero 
      adhocRTuserIDs*)
      if (tweetA.TweetRecord._userid_ == tweetB.TweetRecord._adhocRTuserID_) 
	|| (tweetA.TweetRecord._username_ == tweetB.TweetRecord._adhocRTusername_) then
	(let timeA = TweetRecord.tm_fromString tweetA.TweetRecord._time_string_ in
	 let timeB = TweetRecord.tm_fromString tweetB.TweetRecord._time_string_ in
	 GenericUtility.precedes timeA timeB)
      else false in
    let rec checkall ~lista ~elem ~htbl = 
      match lista with
	h :: t -> 
	  begin
	    if isCandidate ~tweetA:h ~tweetB:elem then
	      begin
		Hashtbl.add htbl h elem;
	      end
	    else if isCandidate ~tweetA:elem ~tweetB:h then
	      begin
		Hashtbl.add htbl elem h;
	      end
	    else ();
	    checkall ~lista:t ~elem:elem ~htbl:htbl;
	  end
      | [] -> htbl in
    (*Now perform comparisons against all other elements for every element in listB*)
    let rec helper ~lista ~listb ~htbl = 
      match listb with
	h :: t -> let updatedHtbl = checkall ~lista:lista ~elem:h ~htbl:htbl in
		  let updatedHtbl2 = checkall ~lista: listb ~elem:h ~htbl:updatedHtbl in
		  helper ~lista:lista ~listb:t ~htbl:updatedHtbl2
      | [] -> htbl in
    helper ~lista:listA ~listb:listB ~htbl:candidates;;
  
  (*Given an adhoc set of tweets, and a set of popular tweets and retweets, 
    creates a new hashtable mapping potential flows from known popular tweets & 
    retweets to adhoc retweets. Eyeball these and then work on a greatest common
    substring or other text matching scheme to eliminate false positives.
    val main : unit -> unit *)
  let main () =
    let archive = Sys.argv.(1) in   (* eg "a bz2 file"*)
    let oldhtbl = Sys.argv.(2) in   (* eg "output.txt" from older module run*) 
    let outfile = Sys.argv.(3) in   (* eg "histogram.txt"*)
    let outfile2 = Sys.argv.(4) in   (* eg "fattbl.txt", to print known tweets and retweets with text & other datums*)
    let outfile3 = Sys.argv.(5) in  (* eg "firstrun.txt", place to write possible retweet flows*)
    let tuple = submain ~infile:archive ~oldhtblfile:oldhtbl ~outfile:outfile ~outfile2:outfile2 in
    let htbl = GenericUtility.fst3 triple in
    let tweets = GenericUtility.fst tuple in 
    let adhocset = GenericUtility.snd tuple in
    begin
      submain ~infile:archive ~oldhtblfile:oldhtbl ~outfile:outfile ~outfile2:outfile2;
      print_string "===Submain () worked...attempting to find flows...";
      let candidatemappings = constructCandidateFlows ~fatset:tweets ~adhocset:adhocset in
      TweetRecord.printHTable ~hashtbl:candidatemappings ~outfile:outfile3;
    end;;  
  
  main ();;

(*
  Note that it is easy to recover userids, not just names, from the dataset json, 
  for adhoc retweets, specifically also for the RT @ username. Although it turns
  out that for some reason the twitter-generated json archive data is not 
  consistent: sometimes the user_mentions field has the user names and user ids
  of the RT @ username we want, other times it does not despite the presence 
  within the text. Ignore such cases. Also, sometimes it is because the RT @ 
  username has been concatenated by the user with the text and it is impossible
  to identify the start of the text and end of the username without more tools
  at our disposal.
  We ignore adhoc retweets that have multiple "RT"s and usernames, of which some 
  proportion exists.
  Also note that since an adhoc retweet might be a retweet of another, we're not
  going to find all possible flows unless we explore all possible combinations...
  which we don't.
  We just look for adhoc retweets of known tweets & retweets & adhoc retweets.
  Search for any known tweet in the set whose userid matches the that of the 
  username mentioned in the text. Check that timestamps do not preclude it from
  being an adhoc retweet. Finally, add to a new htbl as a new mapping for later 
  eyeballing. Eventually we'll apply some greatest common substring or locality 
  hash upon text.
  ===== For the smallest set of March 11====
  No. of Popular tweets & retweets found: 15,285
  No. of AdHoc tweets (with only 1 RT@username) found: 16,924
*)
end
