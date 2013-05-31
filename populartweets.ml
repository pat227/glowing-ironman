(*====================untested code here down===================================*)

(*This short module needed to provide an ordered type so we can make sets and maps in next module.
  Mutable fields required to allow us to fill in table with missing datums from second pass 
  over compressed archive file; user id as string can be used at first to spot confirm correctness by inspection. *)
module TweetRecord : sig 
  type t = { _id_ : int; _userid_: int ; _username_: string ; 
	    _time_string_: string; _text_:string };;
  val compare: t -> t -> int
  val print_FatTweetRecord : outchan:'a -> trec:t -> unit
  val printHTable : hashtbl:(t, t) Hashtbl.t -> outfile:string -> unit
end = struct
  type t = { _id_ : int; _userid_: int ; _username_: string ; 
	    _time_string_: string; _text_:string };;

  let compare t1 t2 = Int64.compare (Int64.of_int t1._id_) (Int64.of_int t2._id_);;

  (*Print to screen*)
  let print_FatTweetRecord ~outchan ~trec = 
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
      print_string trec._time_string_;
      print_string " ";
      print_newline ();
    end;;

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
	  output_string outchan "\n";
	end
    in
    begin
      Hashtbl.iter (writeFold outchan) hashtbl;
      close_out outchan;
    end;;
end


(*Reconstruct some of the table created above, from a file--should have a manner of doing so live as well--and
  then fill in tweet text, user ids and names, etc, and try to find flows via adhoc retweets by examining the
  entire dataset again; this cannot be done for text b/c memory pressure even for a relatively "small" data set
  is enormous even with more fine grained memory management. *)
module Followup = struct
  let popular_enough = 101;;
  module Tweetset = Set.Make(TweetRecord);;

  (*A good amount of this module and the one above share some functions that will be factored out into
    a common dependency to avoid duplication, bloat, etc.*)

  (*The older type being recovered from the input file that has less datums*)
  (*type tweet_recordOLD = {id : int; time_string: string;};;*)
  (*let tweets_HASHtbl_OLD : (tweet_recordOLD, tweet_recordOLD) Hashtbl.t = Hashtbl.create(8192);; *)
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
	  (let hasarrow = String.contains line '>' in
	   let listlength = List.length accum in
	   let int32listlength = Int32.of_int listlength in
	   let updatedMap = try 
			      FreqMap.add int32listlength (Int32.add (Int32.of_int 1) (FreqMap.find int32listlength map)) map 
	     with Not_found -> FreqMap.add int32listlength (Int32.of_int 1) map in
	   match hasarrow with true ->
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
	    let updatedSet2 = updateSet ~set:updatedSet ~tweet:retweet in
	    consumeBuf ~stringbuf:stringbuf ~startpos:(nextbreak+1) ~set:updatedSet2;
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
	  print_string "===Finished creating popular tweets set===";
	  updatedset;
	end
    in
    helper ~set:set ~bz2inchan:bz2inchan ~chunksize:8388608 ~leftover:"";;
  
  (*Took a short while to get the next line exactly correct*)
  let tweets_HASHtbl_NEW : (Tweetset.t, Tweetset.elt) Hashtbl.t = Hashtbl.create(8192);;

  (*Using the Set of tweet & retweet (not ad hoc) records that contain text, 
    time, username, userid and the old hashtable of mappings, construct a new
    hashtable of mappings.
    val constructNewHtbl : set:Tweetset.t -> 
    oldHtbl:(TweetRecord.t, TweetRecord.t) Hashtbl.t -> 
    newHtbl:'a -> unit  *)
  let constructNewHtbl ~set ~oldHtbl ~newHtbl = 
    let setref = ref set in 
    let semiequal tweetA tweetB = 
      if tweetA.TweetRecord._id_ == tweetB.TweetRecord._id_ && 
	tweetA.TweetRecord._time_string_ == tweetB.TweetRecord._time_string_ 
      then true else false in
    let toiter ~newHtbl ~set key binding =
      let sets = Tweetset.partition (semiequal binding) !set in
      let toInsert = GenericUtility.fst sets in
      let sets2 = Tweetset.partition (semiequal key) !set in
      let key2use = GenericUtility.fst sets2 in
      begin
	if Tweetset.cardinal toInsert == 1 then 
	  (Hashtbl.add newHtbl key2use (Tweetset.choose toInsert);
	   (*update the ref in next line; do not use sets2, we need the set to 
	     always hold the key since we're dealing with multiple bindings; 
	     otherwise we'd get back an empty set upon next iter and partition 
	     and have no key to use at all in above line.*)
           set := GenericUtility.snd sets)
	else
	  print_string "===This should never happen, tweetIDs are supposed to be unique!===";
      end
    in
    let rec helper ~set ~oldHtbl ~newHtbl = 
      let williter = toiter ~newHtbl:newHtbl ~set:set in
      Hashtbl.iter williter oldHtbl
    in
    helper ~set:setref ~oldHtbl:oldHtbl ~newHtbl:newHtbl;;

  (*Invoke functions that parse the tweet -> retweet mapping file; finds
    popular tweets; for each popular tweet retrieves additional datums such
    as text, userid, username; creates a new hashtable with these additional
    datums for these popular tweets.*)
  let main ()=
    let infile = Sys.argv.(1) in   (* eg "a bz2 file"*) 
    let outfile = Sys.argv.(2) in   (* eg "histogram.txt"*)
    let triple = parseTable4PopularTweets ~file:infile in
    let oldhtbl = GenericUtility.fst3 triple in
    let thinset = GenericUtility.snd3 triple in
    let freqmap = GenericUtility.thd3 triple in
    let fatset = fillSet ~set:thinset ~compressedfile:infile in
    let newHtbl = constructNewHtbl ~set:fatset ~oldHtbl:oldhtbl ~newHtbl:tweets_HASHtbl_NEW in
    begin
      GenericUtility.print2Afile_Int32Map ~amap:freqmap ~outfile:outfile;
      (newHtbl, fatset);
    end;; 
  
  (*main ();*)

  (*Determine more quickly if a line of text has a nested element indicative of being a non-adhoc retweet
    val isRT : line:string -> int*)
  let isRT ~line =
    let regexp_retweet = Str.regexp "retweeted_status" in
    try Str.search_forward regexp_retweet line 0 with Not_found -> -1;;
  


  (*Do yet another pass over a compressed bz2 tweets file; grab only those tweets that are adhoc
    retweets and collect them only if they MIGHT be adhoc retweets of already known popular
    tweets or retweets, ie, only if RT username matches that of a known user of a known tweet. 
    Also note that it is easy to recover userids, not just names, from the dataset.*)
  let parse4AdHocRetweets ~file ~fathtbl =
    let bz2inchan = openBZ2file ~file:file in
    (*Similar but only invoke upon contents of text field within json, not entire json!
      Keeping things simple for now; only consider ad hoc retweets with a single "RT".
      val isAdHocRT : line:string -> bool*)
    let sameuserid tweetA tweetB = 
      if tweetA.TweetRecord._id_ == tweetB.TweetRecord._id_
      then true else false in
    (*input looks like: Mar 11 12:39:05 2013*)
    let laterTweet tweetA tweetB = 
      let timeA = tweetA.TweetRecord._time_string_ in
      let timeB = tweetB.TweetRecord._time_string_ in
      (*each of the form: "Mar 11 12:39:05 2013"*)
      let space1A = (String.index_from timeA 0 ' ') + 1 in
      let space2A = (String.index_from timeA space1 ' ') + 1 in
      let space3A = (String.index_from timeA space2 ' ') + 1 in
      let space4A = (String.index_from timeA space3 ' ') + 1 in
      let space5A = (String.index_from timeA space4 ' ') + 1 in
      let length = String.length timeA in


    let isAdHocRT ~line = 
      let j = try
		(*print_string "===Parsing line: "; print_string line; print_newline ()*)
		Yojson.Basic.from_string line;
	with _ -> (print_string "===Failed to Parse JSON: "; print_string line; print_newline (); `Null)
      in
      let text = Yojson.Basic.Util.member "text" j in 
      (*Other possible regexps:  (RT.?\u[a-z0-9]{3,4}.?.?@) or better  (RT[^@]{0,5}@) *)
      let regexp_retweet = Str.regexp "RT[^@]{0,9}@" in
      let pos = try Str.search_forward regexp_retweet text 0 with Not_found -> -1 in
      let pos2 = try Str.search_forward regexp_retweet text pos with _ -> -1 in
      match pos, pos2 with
	-1, -1 -> false (*not an adhoc retweet*)
      |  _, -1 -> true  (*adhoc retweet we're looking for*)
      | -1,  _ -> false (*impossible*)
      |  _,  _ -> false (*too many RT's, hadn't realized many existed of this form*)
	if pos >=0 then true else false in
    let rec consumeBuf ~stringbuf ~startpos =
      let nextbreak = try 
			String.index_from stringbuf startpos '\n' 
	with _ -> (print_string "===Consumed 8MB buffer with a prior startpos of: ";
		   print_string (string_of_int startpos);
		   print_newline ();
		   String.length stringbuf) in
      if nextbreak == (String.length stringbuf) then 
	startpos
      else
	let toconsume = String.sub stringbuf startpos (nextbreak - startpos) in
	

  
end