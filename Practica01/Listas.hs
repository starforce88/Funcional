--Lozada Mendez Ivan--
module Listas where

	--Funcion atN--
	atN::[a]->Int->a
	atN [] _ = error "atN No existe la entrada en la lista"
	atN (x:xs) n 
		| n == 1 = x
		| otherwise = atN xs (n-1)
		
	--Funcion minlista (k primeros elementos de una lista, donde k es el elemento m´as peque˜no de la lista)--
	minlista::[Int]->[Int]
	minlista [] = []
	minlista (x:xs) = auxrec (auxmin (x:xs)) (x:xs)

	--auxmin (Buscar el minimo de una lista)--
	auxmin::[Int]->Int
	auxmin [x] = x
	auxmin (x:xs) 
		| x < auxmin xs = x
		| otherwise = auxmin xs

	--auxrec recorre la lista hasta el elemento n--
	auxrec::Int->[Int]->[Int]
	auxrec 0 _ = []
	auxrec n (x:xs) 
		| n == 1 = [x]
		| otherwise = x: auxrec (n-1) xs

	--parlista --