SHELL=/bin/bash
main:
	ocamlbuild -use-menhir -pkgs oUnit,str,graphics,ANSITerminal main.byte

play:
	ocamlbuild -use-menhir -pkgs oUnit,str,graphics,ANSITerminal main.byte && ./main.byte


clean:
	ocamlbuild -clean

zip:
	zip src.zip *.ml{,i,y,l}

testship:
	ocamlbuild -use-menhir -pkgs oUnit,str,ANSITerminal testship.byte && ./testship.byte

testplayer:
	ocamlbuild -use-menhir -pkgs oUnit,str,ANSITerminal testplayer.byte && ./testplayer.byte

testboard:
	 ocamlbuild -use-menhir -pkgs oUnit,str,graphics,ANSITerminal testboard.byte && ./testboard.byte

