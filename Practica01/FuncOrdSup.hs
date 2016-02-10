--Lozada Mendez Ivan--
module FuncOrdSup where
	--Tribonacci--
	trib::Int->[Int]
	trib 1 = [tribaux 1]
	trib n = trib (n-1)++[tribaux n]

	--Tribonacci auxiliar--
	tribaux::Int->Int
	tribaux 1 = 1
	tribaux 2 = 1
	tribaux 3 = 2
	tribaux n = tribaux (n-1)+tribaux (n-2)+tribaux (n-3)