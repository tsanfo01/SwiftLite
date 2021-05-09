.PHONY : toplevel.native
toplevel.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 toplevel.native

.PHONY : test
test : toplevel.native
	python3.7 script.py

.PHONY : clean
clean :
	ocamlbuild -clean