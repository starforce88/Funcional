--Lozada Mendez Ivan--
module Digitos where

	--Ejercicio 4--
	sdigito::Integer->Integer
	sdigito n 
		| n < 10 = n
		| otherwise = sdigito((mod n 10)+(sdigito(div n 10)))

	sumaConcat::Integer->Integer->Integer
	sumaConcat n k
		| k == 1 = (sdigito n)
		| otherwise = sdigito((sdigito n)+(sumaConcat n (k-1)))--

	--Ejercicio 5--
	iflip::Integer->Integer
	iflip n 
		| n < 10 = n
		| otherwise = ((mod n 10)*(10^((digitos n)-1))) + (iflip (div n 10))

	digitos::Integer->Integer
	digitos  n
		| n < 10 = 1
		| otherwise = 1 + digitos (div n 10)

	existeaux::Integer->[Integer]->Integer
	existeaux n [] = 0
	existeaux n (x:xs)
		| (x + (iflip x)) == n = x
		| (x + (iflip x)) < n = existeaux n xs
		| (x + (iflip x)) > n = existeaux n xs

	existe::Integer->Integer
	existe n = existeaux n [1..n]