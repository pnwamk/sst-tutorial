test:
	raco make -v index.scrbl
	raco test model/testing.rkt

html:
	raco make -v index.scrbl
	scribble --dest docs --html index.scrbl

clean:
	rm -fr docs/*

#test-ci:
#	raco test --drdr --timeout +inf.0 -j 4 --package redex-aam-tutorial

#install:
#	raco pkg install ../redex-aam-tutorial
