import Data.Char
    
suma :: Int -> Int -> Int
suma x y = x + y

aASCII :: Char -> Int
aASCII x = ord x
--------------------------------------    TUPLAS    -------------------------------------------
divEntera:: (Int, Int) -> (Int, Int)
divEntera (m, n) = (m `div` n, m `rem` n)

--Dados dos intervalos de numeros enteros diga si hay interseccion entre ellos
interseccionIntervalos :: (Int,Int) -> (Int,Int)->Bool
interseccionIntervalos (x,y) (z,w) = not (w <=y)

--Dados tres numeros enteros, obtener el mayor de ellos (con guardas)
mayor ::(Int, Int, Int) -> Int
mayor(x,y,z)
    | x >= y && x >=z = x
    | y > z = y
    | otherwise = z

--Dados dos numeros enteros, obtener el mayor de ellos (con guardas)
mayorDeDos ::Int-> Int-> Int
mayorDeDos x y
    | x > y = x
    | otherwise = y

-- El mayor de tres numeros usando la funcion anterior implícita
mayorDeTres:: Int -> Int -> Int -> Int
mayorDeTres x y = mayorDeDos x.mayorDeDos y


--------------------------------------------- LISTAS ----------------------------------------------

--Dados dos numeros devolver una lista con el resultado de sumar , restar y multiplicar ambos numeros
operaciones :: Int -> Int -> [Int]
operaciones x y = [x+y,x-y,x*y]

-- Dadas dos cadenas de caracteres, si la longitud de ambos es menor o igual a tres, delvolver la union o sino vacio
posibleUnion :: [Char]->[Char]->[Char]
posibleUnion x y
    | length [x] <= 3 && length [y] <= 3 = x++y
    | otherwise = []

-- Lo mismo pero con if
posibleUnion' :: String -> String -> String
posibleUnion' x y = if length x  <= 3 && length y <=3 then x++y else []

-----------  LISTAS POR COMPRENSION   -------------------
lista100 :: [Int]
lista100 = [x*x | x <- [1..10]]

-- Dada una frase, devolver la cadena con solo las mayusculas existentes
mayusculasFrase :: [Char] -> [Char]
mayusculasFrase x = [aux | aux <- x , isUpper aux]


---------------------------------       PATRONES         ------------------------------------------------------

sumaPatron :: [Int] -> Int
sumaPatron [] = 0
sumaPatron [x,y,z] = x+y+z

primeraConUno :: [(Int,Float)]-> (Integer,Float)
primeraConUno ((1,x):xs) = (1,x)
primeraConUno (x:xs) = primeraConUno xs


--Dada una lista, devolver true si tiene menos de 3 elementos
listaMenorTres :: String -> Bool
listaMenorTres (c1:c2:c3:c4:x) = False

listaMenorTres' :: String -> Bool
listaMenorTres' [] = True
listaMenorTres' [x] = True
listaMenorTres' [x,y] = True
listaMenorTres' _ = False


--Dada una cadena devolver si comienza o no por mayuscula
primeraEsMayuscula :: [Char] -> Bool
primeraEsMayuscula [] = False
primeraEsMayuscula x = isUpper (head x)

primeraEsMayuscula' :: String -> Bool
primeraEsMayuscula' (x:_) = isUpper x
primeraEsMayuscula' _ = False

--Dada una cadena, saber si el primer caracter es 'a' o 'A'
primeraEsA :: String -> Bool
primeraEsA ('a':_) = True
primeraEsA ('A':_) = True
primeraEsA _ = False

-- Dadas dos cadenas de caracteres devolverlas ordenadas alfabeticamente segun su primer caracter, si una está vacia, ira primero
--ordenFrasesAlfabeticamente :: (String , String ) -> (String, String)
--ordenFrasesAlfabeticamente [(x:xs), (y:ys)]
--    | ord y <= ord x = (ys,xs)
  --  | otherwise (xs,ys)
--ordenFrasesAlfabeticamente xs , ys = if xs == [] then (xs,ys)
--ordenFrasesAlfabeticamente xs , ys = if ys == [] then (ys,xs)

ordenFrasesAlfabeticamente' :: String -> String -> (String , String)
ordenFrasesAlfabeticamente' [] lista = ([],lista)
ordenFrasesAlfabeticamente' lista [] = ([],lista)
ordenFrasesAlfabeticamente' c1@(x:_) c2@(y:_) =if  (toLower x) <= (toLower y ) then (c1,c2) else (c2,c1)







-------------------------------          RECURSIVIDAD          ----------------------------------------

--Hacer una funcion calcular la longitud de una lista de numeros enteros. Hacerlo con una unica ecuacion, con varias + ajustes de patrones y con expresion de CASE
----Una unica ecuacion
listaEnteros :: [Int]-> Int
listaEnteros x = if x == [] then 0 else (1+ listaEnteros (tail x))
-----Varias + ajuste de patrones
listaEnteros' :: [Int] -> Int
listaEnteros' [] = 0
listaEnteros' (_:xs) = (1+ listaEnteros xs) 

-----Expresion con CASE


-- Funcion recursuca que dada una cadena  y un caracter, cuente el numero de apariciones de ese caracter
------ Con recursividad no final
numeroRepeticiones :: String -> Char -> Int
numeroRepeticiones x y = if x == [] then 0 else (if head x == y then 1 + numeroRepeticiones (tail x) y else 0 + (numeroRepeticiones (tail x) y))

------ Con recursividad final (siempre que sea final , habra minimo 2 funciones)
numeroRepeticiones' :: [Char] -> Char -> Int  --Utiliza un contador para saber el numero de apariciones
numeroRepeticiones' frase cont = numeroRepeticionesAux' frase cont 0

numeroRepeticionesAux' :: [Char] -> Char -> Int -> Int
numeroRepeticionesAux' [] _ cont = cont
numeroRepeticionesAux' (x:xs) c cont = if (x==c) then numeroRepeticionesAux' xs c (cont+1) else numeroRepeticionesAux' xs c cont

-- Funcion que dada una lista de elementos devuelva la suma de todos ellos
------- Con recursividad no final
sumaElementosRecursiva :: String -> Int
sumaElementosRecursiva x = if x == [] then 0 else (1 + sumaElementosRecursiva (tail x))

------- Con recursividad final
sumaElementosRecursiva':: [Char] -> Int
sumaElementosRecursiva' x = sumaElementosRecursivaAux x 0

sumaElementosRecursivaAux :: [Char] -> Int -> Int
sumaElementosRecursivaAux [] cont = cont
sumaElementosRecursivaAux (_:xs) cont = sumaElementosRecursivaAux xs (cont+1) 







---------------------------              FUNCIONES ORDEN SUPERIOR              ----------------------------------------

dosVeces :: (Int -> Int) -> Int -> Int
dosVeces f x = f (f x)







--------------------------------      LISTADO EJERCICIOS 3.0     -------------------------------------

-- 1: Dados 2 numeros enteros, mostrar el mayor de entre el cociente y el resto de la division
componer :: Int -> Int -> Int
componer x y
    | div x y < mod x y = mod x y
    | otherwise = div x y
    

-- 2: Dado un numero devolver su sucesor (siguiente numero)
sucesor :: Int -> Int
sucesor x = x+1

--3: Dado un numero devolver cuádruple de este. Hacerlo con una funcion previa que calcule el doble de un numero
doble :: Int -> Int
doble x = x*2

cuadruple :: Int -> Int
cuadruple x = doble (doble x) 







---------------------------------   LISTADO  EJERCICIOS 3.1     -------------------------------------------------------


--1: Dado 3 numeros determinar si está ordenado de menor a mayor o no
ordenadosMenor:: Int -> Int -> Int -> Bool
ordenadosMenor x y z
    | x < y && y < z = True
    | otherwise = False

-- 2: Dada una tupla de 3 numeros la devuelva ordenada de menor a mayor
ordenarTupla :: (Int, Int , Int) -> (Int , Int , Int)
ordenarTupla (x,y,z)
    | x < y && y < z = (x,y,z)
    | z > x && y > z = (x,z,y)
    | y < x && x < z = (y,x,z)
    | z > y && z < x = (y,z,x)
    | z < x && x < y = (z,x,y)
    | otherwise = (z ,y,x)

-- 3: Recibiendo un numero real devolver una tupla con su parte entera y sus dos primeros decimales
descomponerReal:: Float -> (Int, Int )
descomponerReal x = (truncate x, truncate(((x - fromInteger(truncate x) )*100)))

-- 4 Dado un numero entero, devolver la lista de sus divisores mediante listas de comprension
divisores:: Int -> [Int]
divisores x = [aux | aux <- [1..x], x `rem` aux ==0]

-- 5: Devolver si el caracter dado es un digito o no

    --a: Con guardas
esDigito :: Char -> Bool
esDigito x 
    | isDigit x = True
    | otherwise = False

    --b: Con ajuste de patrones
esDigito' :: Char -> Bool
esDigito' x = isDigit x

-- 6: Saber si un numero es primo o no ( usar la funcion del ejercicio 5)
esPrimo :: Int -> Bool
esPrimo x
    | length (divisores x) == 2 = True
    | length (divisores x) == 1 = True
    | otherwise = False

-- 7: Dada una lista de enteros devolver otra lista con los elementos que son impares y los primos, con lista por comprensión
listaPrimosImpares ::[Int] -> [Int]
listaPrimosImpares x = [ aux | aux <- x, esPrimo aux && aux `rem` 2 == 1  ]

--8: Dado un numero, devolver los primos menores de ese numero
primosMenorIgual:: Int -> [Int]
primosMenorIgual x =  [ aux | aux <- [1..x], esPrimo aux && aux <= x ]

--9: Dadas dos tuplas, desencriptar el mensaje con las siguientes pistas:
-----Si la letra de la segunda tupla es vocal , se coge la letra de la primera, sino nada

codificacionTuplas :: [(Char,Char)] -> String
codificacionTuplas x =  [ aux1 | (aux1,aux2)<- x, aux2 == 'a' || aux2 == 'e'  || aux2 == 'i'  || aux2 == 'o' ||  aux2 == 'u' ]

--10: Dado un numero de enteros n y una lista de 2 tuplas de enteros, devolver una lista con las tuplas donde el primer elemento sea impar y mayor que el numero que le pasas
filtrarTuplas :: [(Int, Int)] -> Int -> [(Int, Int)]
filtrarTuplas x c = [(aux1 , aux2) | (aux1, aux2) <- x , aux1 `rem` 2 == 1 && aux1 > c]

--11: Saber el numero de lista de tres tuplas que son pitagoricas (x^2 + y^2 = z^2) con listas de comprension
cuantasPitagoricas :: [(Int,Int,Int)]-> Int
cuantasPitagoricas x = length[ aux1 | (aux1,aux2,aux3)<- x,  aux1*aux1 + aux2*aux2 == (aux3*aux3)]

--12: Funcion que determine si una letra es minuscula o mayuscula sin usar isUpper
esMayuscula :: Char -> Bool
esMayuscula x = if (ord x) >= 65 && (ord x) <= 90 then True else False

--13 Dado un String, cambiar minusculas por mayusculas y viceversa
mayusculaMinuscula:: String -> String
mayusculaMinuscula x = [if esMayuscula aux then toLower aux else toUpper aux| aux <- x ]

--14: Dadas dos cadenas de caracteres, devolver una lista con los ASCII de esos caracteres con listas de comprension
listaASCII :: String -> [Int]
listaASCII x = [ ord aux | aux <- x ]

--15: Dada una lista de enteros, devolver un mensaje con el primer elemento y la longitud de la lista
mensajeLista:: String -> String
mensajeLista x = "Primer elemento: "++ show (head x) ++",longitud "++ show (length x)

-- 16: Funcion para calcular el numero de mayusculas con listas de comprension
contarMayusculas :: String -> Int
contarMayusculas x = length[ aux| aux <- x, isUpper aux]





-----------------------------    LISTADO  EJERCICIOS 3.2    ----------------------------------------------------
--1: Dada una cadena de caracteres un caracter, devolver el numero de veces que aparece el caracter, usar ajuste de patrones y lista de comprension
contarApariciones :: String -> Char -> Int
contarApariciones [] _  = 0
contarApariciones x c = length[aux |aux <- x, aux==c ]

--TODO2: Dada una tupla compuesta de una tupla de cadena de caracteres y numero , devolver el primer elemento de cada tupla, con ajuste de patrones
--manipulaTuplas :: ((String, Int), (String, Int),(String, Int)) -> (String,String,String)
--manipulaTuplas (x,y,z) = (head x, head y, head z)

--3: Funcion que recibe una lista y da true si la suma de los 4 primeros elementos es menor que 10, con ajuste de patrones
sumaMenor10 :: [Int] -> Bool
sumaMenor10 [] = False
sumaMenor10 (x1:x2:x3:x4:_) = if x1+x2+x3+x4 < 10 then True else False

--4: Dado un caracter, devolver si es un punto cardinal y mostrar cual
puntoCardinal :: Char -> String
puntoCardinal x = case toUpper x of
    'N' -> "Norte"
    'S' -> "Sur"
    'O' -> "Oeste"
    'E' -> "Este"
    _ -> "El caracter introducido no pertene a un punto cardinal"

--5: Dado un numero y una lista saber si el numero esta todas las veces dentro de la lista. Sin usar recursividad
todosIguales :: Int -> [Int] -> Bool
todosIguales n [] = False
todosIguales n x = if length[aux |aux <- x, aux ==n ] == length x then True else False

--6: Funcion que devuelva la primera y la ultima letra de una oración
mensajeFrase :: String -> String
mensajeFrase x = "La primera letra de la frase es"++show (head x) ++", y la ultima letra es: "++show (last x)


--7: Dado un entero delvolver un mensaje indicando el rango (menor 10, entre 10 y 20 o mayor de 20)
clasificarValorEntrada:: Int -> String
clasificarValorEntrada x
    | x < 10 = "El valor de entrada es menor que 10"
    | x >= 10 && x <= 20 = "El valor está entre 10 y 20"
    | x > 20 = "El valor es mayor que 20"
    | otherwise = "Valor lo reconocido"

--11: Devolver si dos listas son iguales comparando valor a valor
listasIguales :: [Int] -> [Int] -> Bool
listasIguales x y = if length x /= length y then False else if (length [aux1 | (aux1,aux2) <-zip x y , aux1 == aux2 ] == length y) then True else False



{-
----------------------------            TIPS               ---------------------------------

--PARA IF ELSE O SIMILAR:
----Lo que se ponga en la misma linea lo lee como funciones diferentes

--LISTAS
[1...5] = 1,2,3,4,5
[1,3...12] = 1,3,5,7,9,11
[1,4...] = [1,4,7,11.... (infinito)]

-- Recursividad: 
- Si en la ultima llamada recursiva ya se tiene el resultado final entonces es RESCURSIVIDAD final
- Si hace falta hacer la vuelta al inicio una vez se ha llegado al caso base de la recursividad, es un una RECURSIVIDAD NO final
-}