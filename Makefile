EXECUTABLES = a.out
all: $(EXECUTABLES)
#need to add populartweets.ml and the twitsparer.ml files
GenericUtility.cmx: 
	ocamlopt -c GenericUtility.ml
SeperateTweets.cmx: GenericUtility.cmx
	ocamlopt -c str.cmxa SeperateTWEETS.ml
a.out: GenericUtility.cmx SeperateTweets.cmx
	ocamlopt unix.cmxa str.cmxa GenericUtility.cmx SeperateTWEETS.cmx main.ml

clean:
	rm $(EXECUTABLES) *.o *.cmx *.cmi