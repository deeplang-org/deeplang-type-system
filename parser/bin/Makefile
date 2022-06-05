OCAMLC=ocamlc
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
OCAMLCFLAGS=
all :
	$(OCAMLYACC) ParDeeplang.mly
	$(OCAMLLEX) LexDeeplang.mll
	$(OCAMLC) $(OCAMLCFLAGS) -o TestDeeplang BNFC_Util.ml AbsDeeplang.ml SkelDeeplang.ml ShowDeeplang.ml PrintDeeplang.ml ParDeeplang.mli ParDeeplang.ml LexDeeplang.ml TestDeeplang.ml

clean :
	-rm -f *.cmi *.cmo *.o

distclean : clean
	-rm -f LexDeeplang.* ParDeeplang.* LayoutDeeplang.* SkelDeeplang.* PrintDeeplang.* ShowDeeplang.* TestDeeplang.* AbsDeeplang.* TestDeeplang BNFC_Util.ml Makefile
