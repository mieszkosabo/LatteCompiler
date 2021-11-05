GHC        = ghc
BNFC       = bnfc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : latc

# Rules for building the parser.

Parser/AbsLatte.hs Parser/LexLatte.x Parser/ParLatte.y Parser/PrintLatte.hs Parser/TestLatte.hs : Latte.cf
	${BNFC} --haskell --functor -p Parser Latte.cf 

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

latc: Parser/AbsLatte.hs Parser/ParLatte.hs Parser/LexLatte.hs Src/Main.hs Src/Frontend/*.hs
	${GHC} -o latc Src/Main.hs

# Rules for cleaning generated files.

clean :
	-find . -type f -name "*.hi" -exec rm -rf {} +
	-find . -type f -name "*.o" -exec rm -rf {} + 
	-find . -type f -name "*.log" -exec rm -rf {} + 
	-find . -type f -name "*.aux" -exec rm -rf {} + 
	-find . -type f -name "*.dvi" -exec rm -rf {} + 
	-find . -type f -name "*.j" -exec rm -rf {} +
	-find . -type f -name "*.class" -exec rm -rf {} +
	-find . -type f -name "*.bc" -exec rm -rf {} +


distclean : clean
	-rm -rf Parser/

# EOF
