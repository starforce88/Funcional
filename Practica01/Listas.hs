--Lozada Mendez Ivan--
module Listas where

	--Funcion atN--
	atN::[a]->Int->a
	atN [] _ = error "atN No existe la entrada en la lista"
	atN (x:xs) n 
		| n == 1 = x
		| otherwise = atN xs (n-1)
		