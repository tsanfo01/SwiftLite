toplevel.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 toplevel.native

test : clean toplevel.native
	python3.7 script.py

clean :
	ocamlbuild -clean