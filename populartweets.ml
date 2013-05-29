(*====================untested code here down===================================*)

(*This short module needed to provide an ordered type so we can make sets and maps in next module.
  Mutable fields required to allow us to fill in table with missing datums from second pass 
  over compressed archive file; user id as string can be used at first to spot confirm correctness by inspection. *)
module TweetRecord : sig 
  type t = { _id_ : int; _userid_: int ; _username_: string ; 
	    _time_string_: string; _text_:string };;
  type elt = t
  val compare: t -> t -> int
end = struct
  type t = { _id_ : int; _userid_: int ; _username_: string ; 
	    _time_string_: string; _text_:string };;
  type elt = t;;
  let compare t1 t2 = Int64.compare (Int64.of_int t1._id_) (Int64.of_int t2._id_);;
end


(*Reconstruct some of the table created above, from a file--should have a manner of doing so live as well--and
  then fill in tweet text, user ids and names, etc, and try to find flows via adhoc retweets by examining the
  entire dataset again; this cannot be done for text b/c memory pressure even for a relatively "small" data set
  is enormous even with more fine grained memory management. *)
module Followup = struct
  (*let tweets_HASHtbl : (TweetRecord.t, TweetRecord.t) Hashtbl.t;; = Hashtbl.create(4194304);;*)  
  module Tweetset = Set.Make(TweetRecord);;

  (*A good amount of this module and the one above share some functions that will be factored out into
    a common dependency to avoid duplication, bloat, etc.*)

  (*Determine more quickly if a line of text has a nested element indicative of being a non-adhoc retweet
    val isRT : line:string -> int*)
  let isRT ~line =
    let regexp_retweet = Str.regexp "retweeted_status" in
    try Str.search_forward regexp_retweet line 0 with Not_found -> -1;;

  (*Similar but only invoke upon contents of text field within json, not entire json!
  val isAdHocRT : line:string -> bool*)
  let isAdHocRT ~line =
    (*Other possible regexps:  (RT.?\u[a-z0-9]{3,4}.?.?@) or better  (RT[^@]{0,5}@) *)
    let regexp_retweet = Str.regexp "RT[^@]{0,5}@" in
    let pos = try Str.search_forward regexp_retweet line 0 with Not_found -> -1 in
    if pos >=0 then true else false;;

  (*val isFirst : line:string -> bool*)
  let isFirst ~line = 
    let regexp_arrow = Str.regexp "->" in
    let bool = try 
		 Str.search_forward regexp_arrow line 0 
      with Not_found -> -1 in
    if bool >= 0 then true else false;;

  (*The older type being recovered from the input file that has less datums*)
  (*type tweet_recordOLD = {id : int; time_string: string;};;*)
(*  let tweets_HASHtbl_OLD : (tweet_recordOLD, tweet_recordOLD) Hashtbl.t = Hashtbl.create(8192);; *)
  let tweets_HASHtbl_OLD : (TweetRecord.t, TweetRecord.t) Hashtbl.t = Hashtbl.create(8192);;

  (*Clearer intention that just hinging on true/false or other values*)
  type inputstate = 
      Good
    | Bad
    | EndofFile;;

  (*Use return type to avoid potential or illusory incomplete matches*)
  type parse_result = RetweetParsed of Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json *
      Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json * Yojson.Basic.json
    | NoParse;;

  (*val input_next_x_chars_improved :
      bzinchan:Bz2.in_channel -> howmanymore:int -> inputstate * string*)
  let input_next_x_chars_improved ~bzinchan ~howmanymore =
    (*let buf = Buffer.create howmanymore in*)
    let sbuf = String.make howmanymore '!' in (*fillBuffer ~buffer:buf ~numchars:howmanymore in*)
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

  (*Gets id, userid, username, time & text of a tweet & retweet pair
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
	      (*print_string "===Parsed; returning triple..."; 
	      print_string (Yojson.Basic.to_string id);
	      print_string (Yojson.Basic.to_string rid);
	      print_string (Yojson.Basic.to_string rtime);
	      print_newline ();*)
	      (RetweetParsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)))
  
  (*From file with entries that look like:    "310250954186444801, Mar 09 04:49:26 2013"
    val trecord_from_string : trecstring:string -> TweetRecord.t
  *)
  let trecord_from_string ~trecstring = 
    let pos = String.index_from trecstring 0 ',' in
    let id = String.sub trecstring 0 (pos - 1) in
    let id_int = int_of_string id in
    let time = String.sub trecstring (pos + 2) ((String.length trecstring) - pos - 2) in
    { TweetRecord._id_ = id_int; _time_string_ = time; _userid_ = 0 ; _username_ = ""; _text_ = ""; };; 
  
  (*(Parsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)))
    val createTrecord :
    id:Yojson.Basic.json ->
    time:Yojson.Basic.json ->
    userid:Yojson.Basic.json ->
    username:Yojson.Basic.json -> 
    text:Yojson.Basic.json -> TweetRecord.t
  *)
  let createTrecord ~id ~time ~userid ~username ~text =
    let id_s = Yojson.Basic.to_string id in
    let id_i = int_of_string id_s in
    let time_s_proto = Yojson.Basic.to_string time in
    (*"Mon Mar 11 12:39:05 +0000 2013" wastes space, cut it down to: "Mar 11 12:39:05 2013"*)
    let time = (String.sub time_s_proto 5 15) ^ (String.sub time_s_proto 26 5) in
    let userid_s = Yojson.Basic.to_string userid in
    let userid = int_of_string userid_s in
    let username = Yojson.Basic.to_string username in
    let text = Yojson.Basic.to_string text in
    { TweetRecord._id_ = id_i; _time_string_ = time; _userid_ = userid ; _username_ = username; _text_ = text };;

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

  module FreqMap = Map.Make(Int32);; 
    
  (*Parse the file that was a hashtable of keys->mappings; find those tweets with > X retweets mappings; 
    make a set of those only while also reconstructing the original hashtable.
    val parseTable4PopularTweets :
    file:string -> (Tweetset.elt, Tweetset.elt) Hashtbl.t * Tweetset.t * int32 FreqMap.t
  *)
  let parseTable4PopularTweets ~file ~outfile =
    let inchan = open_in file in
    let outchan = open_out outfile in 
    let regexp_arrow = Str.regexp " -> " in
    let rec helper ~inchan ~accum ~htbl ~set ~map =
      let nextline = try (Some (input_line inchan)) with End_of_file -> (close_in inchan; None) in
      match nextline with 
	  None -> (htbl, set, map) (*if nextline in file is new key mapping, add to table prior key/elements 
		       mapping if exceeds cutoff*)
	| Some line -> 
	  (let hasarrow = String.contains line '>' in
	   let listlength = List.length accum in
	   let int32listlength = Int32.of_int listlength in
	   let updatedMap = try 
			      FreqMap.add int32listlength (Int32.add (Int32.of_int 1) (FreqMap.find int32listlength map)) map 
	     with Not_found -> FreqMap.add int32listlength (Int32.of_int 1) map in
	   match hasarrow with true ->
	     if listlength > 21 then
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
	      helper ~inchan:inchan ~accum:updatedaccum ~htbl:htbl ~set:set ~map:updatedMap))
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
    { TweetRecord._id_ = tweet.TweetRecord._id_; _time_string_ = tweet.TweetRecord._time_string_; 
      _userid_ = 0 ; _username_ = ""; _text_ = ""; }
      
  (*Given a hashtable (reconstructed from a file & whose contents were the output 
    of the first module, ParseBZ2Tweets) and a set (containing records within the 
    hashtable) go back to the compressed archive and 1) update the set entries
    with userids, usernames, and tweet text 2) when done, make another hashtable
    using the records in the set using the original hashtable to determine 
    key->mappings, then forget about the original hashtable, just keep the new one 
    and the set *)
  (*Build up the map; specifically fill in missing values within map entries
    val fillSet : set:Tweetset.t -> compressedfile:string -> Tweetset.t   *)
  let fillSet ~set ~compressedfile =
    let bz2inchan = openBZ2file ~file:compressedfile in
    let updateSet ~set:set ~tweet:tweet = 
      let toreplace = truncatedTweet ~tweet:tweet in 
      if (Tweetset.mem toreplace set) then
	let newset = Tweetset.remove toreplace set in
	Tweetset.add tweet newset
      else (*Must not have been popular tweet/retweet, so ignore it*)
	set in
    let rec consumeBuf ~set ~stringbuf ~startpos =
      let nextbreak = try 
			String.index_from stringbuf startpos '\n' 
	with _ -> (print_string "===Consumed 8MB buffer with a prior startpos of: ";
		   print_string (string_of_int startpos);
		   print_newline ();
		   String.length stringbuf) in
      if nextbreak == (String.length stringbuf) then 
	(set, startpos)
      else
	let toconsume = String.sub stringbuf startpos (nextbreak - startpos) in
	let tuple = getRetweetsExtraInfo ~line:toconsume in
	match tuple with 
	  (*(Parsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)))*)
	  (RetweetParsed (id, time, userid, username, text, rid, rtime, ruserid, rusername, rtext)) -> 
	    let sourcetweet = createTrecord ~id:rid ~time:rtime ~userid:ruserid ~username:rusername ~text:rtext in
	    let retweet = createTrecord ~id:id ~time:time ~userid:userid ~username:username ~text:text in
	    let updatedSet = updateSet ~set:set ~tweet:sourcetweet in
	    let updatedSet2 = updateSet ~set:set ~tweet:retweet in
	    consumeBuf ~stringbuf:stringbuf ~startpos:(nextbreak+1) ~set:updatedSet;
	| NoParse -> 
	  consumeBuf ~set:set ~stringbuf:stringbuf ~startpos:(nextbreak+1)
    in
    let rec helper ~set ~bz2inchan ~chunksize ~leftover = 
      let nextchunk = input_next_x_chars_improved ~bzinchan:bz2inchan ~howmanymore:chunksize in
      match nextchunk with
	(Good, toconsume) ->  
	  let merged = leftover ^ toconsume in 
	  let t = consumeBuf ~set:set ~stringbuf:merged ~startpos:0 in
	  let updatedset = GenericUtility.fst t in
	  let leftoverstart = GenericUtility.snd t in
	  let leftover = String.sub merged leftoverstart ((String.length merged) - leftoverstart) in	    
	  helper ~set:updatedset ~bz2inchan:bz2inchan ~chunksize:chunksize ~leftover:leftover;
      | (_, toconsume) -> 
	let t = consumeBuf ~set:set ~stringbuf:toconsume ~startpos:0 in
	let updatedset = GenericUtility.fst t in
	begin
	  print_string "===Finished creating popular tweets set.";
	  updatedset;
	end
    in
    helper ~set:set ~bz2inchan:bz2inchan ~chunksize:8388608 ~leftover:"";;

  
end
