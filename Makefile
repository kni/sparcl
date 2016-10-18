all: p_string_poly p_string_mlton p_substring_poly p_substring_mlton p_haskell p_atto_haskell

p_string_poly: sparcl-string.sml p-string.sml p-string-poly.sml
	polyc -o p_string_poly p-string-poly.sml

p_string_mlton: sparcl-string.sml main.sml p-string.sml p-string.mlb
	mlton -output p_string_mlton p-string.mlb

p_substring_poly: sparcl-substring.sml p-substring.sml p-substring-poly.sml
	polyc -o p_substring_poly p-substring-poly.sml

p_substring_mlton: sparcl-substring.sml main.sml p-substring.sml p-substring.mlb
	mlton -output p_substring_mlton p-substring.mlb

p_haskell: Sparcl.hs p.hs
	ghc -O2 -o p_haskell --make p.hs

p_atto_haskell: p-atto.hs
	ghc -O2 -o p_atto_haskell --make p-atto.hs

clean:
	rm -rf *.o *.hi

realclean: clean
	rm -rf p_string_poly p_string_mlton p_substring_poly p_substring_mlton p_haskell p_atto_haskell
