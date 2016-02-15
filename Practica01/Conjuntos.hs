--Lozada Mendez Ivan--
module Conjuntos where
	--import Eq
	data Set a = ESet | CSet a (Set a) deriving (Read, Show)

	--Funcion listToSet (Convertir una lista a Conjunto)
	listToSet::Eq a => [a]->Set a
	listToSet [] = ESet
	listToSet (x:xs)
		| any (x ==) xs = (CSet x (listToSet (elim x xs)))
		| otherwise = (CSet x (listToSet xs))

	--Funcion elim(Elimina los elementos de una lista que sean iguales al elemento n)
	elim::Eq a => a->[a]->[a]
	elim n [] = []
	elim n (x:xs)
		| n /= x = x:(elim n xs)
		| otherwise = elim n xs

	--Funcion setToList (Convertir conjunto a lista)
	setToList::Eq a => Set a->[a]
	setToList ESet = []
	setToList (CSet a b) = a:setToList b

	--Funcion inSet (Revisa si existe un elemento en el conjunto)
	inSet::Eq a => Set a->a->Bool
	inSet ESet _ = False
	inSet (CSet a b) c 
		| c == a = True
		| otherwise = inSet b c

	--Funcion isEmpty (Verifica si un conjunto es vacio)
	isEmpty::Set a->Bool
	isEmpty ESet = True
	isEmpty (CSet a b) = False

	--Funcion addToSet (Agrega un elemento al conjunto)
	addToSet::Eq a => a->Set a->Set a
	addToSet n ESet = (CSet n ESet)
	addToSet n (CSet c b)
		| n /= c = (CSet c (addToSet n b))
		| otherwise = (CSet c b) 

	--Funcion unionS (Union de conjuntos)
	unionS::Eq a => Set a->Set a->Set a
	unionS a ESet = a
	unionS ESet a = a
	unionS (CSet a b) c
		| inSet c a == True = (unionS b c)
		| otherwise = (unionS b (addToSet a c))

	--Funcion elimElemSet (Eliminar un elemento del conjunto)
	elimElemSet::Eq a => a->Set a->Set a
	elimElemSet a ESet = ESet
	elimElemSet a (CSet b c)
		| a == b = c
		| otherwise = (CSet b (elimElemSet a c))

	--Funcion intersectionS (Interseccion de conjuntos)
	intersectionS::Eq a => Set a->Set a->Set a
	intersectionS a ESet = ESet
	intersectionS ESet a = ESet
	intersectionS (CSet a b) c
		| inSet c a == True = (CSet a (intersectionS b c))
		| otherwise = intersectionS b c

	--Funcion diffS (Diferencia de conjuntos)
	diffS::Eq a => Set a->Set a->Set a
	diffS a ESet = a
	diffS ESet a = ESet
	diffS (CSet a b) c
		| inSet c a == True = (diffS b (elimElemSet a c))
		| otherwise = (CSet a (diffS b c))

	--Funcion subSets (Subconjuntos subSets a b, a es subconjunto de b)
	subSets::Eq a => Set a->Set a->Bool
	subSets a ESet = False
	subSets ESet a = True
	subSets (CSet a b) c
		| inSet c a == False = False
		| otherwise = True && (subSets b c) 

	--Funcion equalsS (Si dos conjuntos son iguales)
	equalsS::Eq a => Set a->Set a->Bool
	equalsS a b = (subSets a b) && (subSets b a)