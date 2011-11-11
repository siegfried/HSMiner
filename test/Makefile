SRC_DIR = ../src

clear:
	rm ${SRC_DIR}/*.{hi,o} *.{hi,o} Test *.tix *.html

test:
	ghc --make -i${SRC_DIR} -fhpc Test.hs
	./Test

assess:
	make test
	hpc markup Test --exclude=Main --exclude=QC
