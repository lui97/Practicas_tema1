--1.Determina el resultado de un número x elevado a una potencia n

elevarNumero :: Int -> Int -> Int
elevarNumero x y= x^y

--2.Determina si un número n se encuentra en un rango determinado

rango:: Int ->Int->Int-> String
rango a b c = if (c >= a && c <=b) then "Esta dentro del rango" else "No esta en el Rango"

--3.Dado un número entero en segundos, determinar la cantidad de horas, minutos y
--segundos que contiene.

segundosAHora :: Integer -> (Integer,Integer,Integer)
segundosAHora s = (horas, minutos, segundos) where 
 horas = div s 3600
 ss = mod s 3600
 minutos = div ss 60
 segundos = mod ss 60

--4. Determine el mayor de 4 enteros
mayor4 :: Int -> Int -> Int -> Int -> String 
mayor4 a b c d= if(a>b && a>c && a>d)
      then "A es mayor"
      else 
	      if(b>a && b>c && b>d)
		     then "B es mayor "
		     else
			      if (c>a && c>b && c>b)
			   	     then "C Es Mayor"
			   	     else
			   	          if (d>a && d>b && d>c)
			   			      then "D es Mayor"

--5.Calcula la suma de una lista
sumaArreglo = sum [5,2,1,6,3,2,5,7]

--6.Determina si un elemento dado está contenido en una lista. Devuelve verdadero o falso
elemnto::Int->Bool 
elemnto a = a `elem` [2,3,4]

--7.Determina si dada una lista, ésta se encuentra ordenada. Se debe devolver verdadero o
--falso

lista_ordenada::Ord a=>[a]->Bool
lista_ordenada [] = True
lista_ordenada [_] = True
lista_ordenada (x:y:xs) = (x<=y) && lista_ordenada (y:xs)
--8. Dadas dos listas, determine si son iguales. Devolver verdadeo o falso.

igualLista:: Eq a => [a]->[a]->Bool
igualLista l1 l2 = l1 == l2

--9.Realizar un función recursiva que retorne como salida el resultado de la suma 1 + 3 + 5
--+ 7 + 9 + N
sumaNumeros :: Int -> Int 
sumaNumeros 0 = 0
sumaNumeros n = n + sumaNumeros(n-2)

--11 Realiza una función en Haskell que permita cargar calcular la unión, intersección y
--diferencia de dos conjuntos datos. Para esto puede hacer uso de la librería “Data.set”
import Data.Set as S 
interseccion :: Ord a => Set a -> Set a -> Set a
interseccion = intersection

diferenciaSimetrica :: Ord a => Set a -> Set a -> Set a
diferenciaSimetrica c1 c2 = 
    (union c1 c2) \\ (intersection c1 c2)
