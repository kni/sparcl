all: p_haskell p_mlton p_poly p_substring_mlton p_substring_poly p_atto_haskell

p_haskell: Sparcl.hs p.hs
	ghc -O2 -o p_haskell --make p.hs

p_mlton: sparcl.sml main.sml p.sml p.mlb
	mlton -output p_mlton p.mlb

p_poly: sparcl.sml p.sml p-poly.sml
	polyc -o p_poly p-poly.sml

p_substring_mlton: sparcl.sml main.sml p-substring.sml p-substring.mlb
	mlton -output p_substring_mlton p-substring.mlb

p_substring_poly: sparcl.sml p-substring.sml p-substring-poly.sml
	polyc -o p_substring_poly p-substring-poly.sml

p_atto_haskell: Sparcl.hs p.hs
	ghc -O2 -o p_atto_haskell --make p-atto.hs

clean:
	rm -rf *.o *.hi

realclean: clean
	rm -rf p_haskell p_mlton p_poly p_substring_mlton p_substring_poly p_atto_haskell
