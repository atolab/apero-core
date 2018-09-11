.PHONY: all clean test doc

BUILD_LIB=dune build	
CLEAN= dune clean
TEST=dune runtest -j1 --no-buffer
DOC=dune build --dev @doc
INSTALL=dune install

all:
		${BUILD_LIB}		

test:
		${TEST}

doc:
	${DOC}

install:
		${INSTALL}

clean:
	${CLEAN}
