EXECUTABLES = a.out twitParser populartweets
all: $(EXECUTABLES)
#need to add populartweets.ml and the twitsparer.ml files
GenericUtility.cmx: 
	ocamlopt -c GenericUtility.ml

SeperateTWEETS.cmx: GenericUtility.cmx
	ocamlopt -c str.cmxa SeperateTWEETS.ml

twitParser: GenericUtility.cmx twitsParser.ml
	ocamlopt -I /usr/local/lib/ocaml/site-lib/easy-format -I /usr/local/lib/ocaml/site-lib/biniou -I /usr/local/lib/ocaml/site-lib/bz2 -I /usr/local/lib/ocaml/site-lib/yojson easy_format.cmx biniou.cmxa bz2.cmxa unix.cmxa str.cmxa yojson.cmx $^ -o twitParser

a.out: GenericUtility.cmx SeperateTWEETS.cmx
	ocamlopt unix.cmxa str.cmxa $^ main.ml

populartweets: GenericUtility.cmx populartweets.ml
	ocamlopt -I /usr/local/lib/ocaml/site-lib/easy-format -I /usr/local/lib/ocaml/site-lib/biniou -I /usr/local/lib/ocaml/site-lib/bz2 -I /usr/local/lib/ocaml/site-lib/yojson easy_format.cmx biniou.cmxa bz2.cmxa unix.cmxa str.cmxa yojson.cmx GenericUtility.cmx populartweets.ml -o populartweets

clean:
	rm $(EXECUTABLES) *.o *.cmx *.cmi

clean-dist:
	rm *.o *.cmx *.cmi