--Lozada Mendez Ivan--
module Listas where

	--Ejercicio 1--
	compara::Eq a => a->[a]->[a]
	compara x [] = [x]
	compara x (y:ys) 
		| x == y = (y:ys)
		| otherwise = (x:y:ys)

	elimDup::Eq a => [a]->[a]
	elimDup xs = foldr compara [] xs

	--Ejercicio 2--
	crea::Eq a => a->Int->[a]->[[a]]
	crea a 0 _ = [[a]]
	crea a n xs = (crea a (n-1) xs)++[a:(take n xs)]

	sublistas::Eq a => [a]->[[a]]
	sublistas [] = []
	sublistas (x:xs) = (crea x (length xs) xs)++sublistas xs

	sumElemLista::[Int]	-> Int
	sumElemLista xs = foldr (+) 0 xs

	sumListas::[[Int]] -> [Int]
	sumListas xs = map (sumElemLista) xs

	maxsumas::[Int]->Int
	maxsumas xs = maximum (sumListas (sublistas xs))

	--Ejercicio 3
	elim::Eq a => a->[a]->[a]
	elim n [] = []
	elim n (x:xs)
		| n /= x = x:(elim n xs)
		| otherwise = elim n xs

	funcion::Eq a => [a]->[a]
	funcion [x] = [x]
	funcion (x:xs)
		| (elim x xs) == [] = []
		| (elim x xs) == xs = x:(funcion xs)
		| otherwise = funcion (elim x xs)