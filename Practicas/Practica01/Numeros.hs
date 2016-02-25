--Lozada Mendez Ivan--
module Numeros  where

	--Funcion max--
	maxi::Integer->Integer->Integer
	maxi a b 
		| b >= a = b
		| otherwise = a

	--Funcion maxthree--
	maxthree::Double->Double->Double->Double
	maxthree a b c 
		| c >= b =
			if c >= a
			then c
			else a
		| otherwise = 
			if b >= a
			then b
			else a

	--Funcion avg (Promedio de 3 numeros)--
	avg::Double->Double->Double->Double
	avg a b c = (a+b+c)/3

	--Funcion gtaveragethree(devuelve el numero que es mayor al promedio de los tres)
	gtaveragethree::Double->Double->Double->Double
	gtaveragethree a b c
		| c >= avg a b c = c
		| b >= avg a b c = b
		| a >= avg a b c = a
		| otherwise = error "gtaveragethree: Ninguno de los numeros es mayor al promedio de los tres"