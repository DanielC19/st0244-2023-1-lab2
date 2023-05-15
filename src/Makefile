# Change Make prefix to ">"
.RECIPEPREFIX = >

# Define GHC compiler and its flags
GHC := ghc
GHCFLAGS := -Werror -Wall -Wmissing-local-signatures -package base -o

# compile and run the program
compile-run :
> $(GHC) $(GHCFLAGS) lab2 Tests.hs
> .\lab2.exe

# just compile the tests file
compile :
> $(GHC) $(GHCFLAGS) lab2 Tests.hs

# run the compiled program
run :
> .\lab2.exe

# clean auto-generated files
clean :
> rm -f *.hi
> rm -f *.o
> find . -type f -executable -delete