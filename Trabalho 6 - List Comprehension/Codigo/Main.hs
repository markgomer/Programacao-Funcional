-- Marco Aurélio Silva de Souza Júnior

{-
1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.
-}
divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1..n], n`mod`x==0 ]

{-
2. Usando List Comprehension escreva uma função, chamada contaCaractere, que conte a ocorrência de um caractere específico, em uma string dada.

contaCaractere :: String -> [(Char, Int)]
contaCaractere str = [(ch, occ) | ch <- (ch:str), occ <- length $ filter (== ch) str, a==ch]
-}

{-
3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.
-}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [x*2 | x <- lista, x>0]

{-
4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.
-}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras l = [(a,b,c) | a <- [1..l], b<-[1..l], c<-[1..l], a^2 == b^2 + c^2]

{-
5. Números perfeitos são aqueles cuja soma dos seus divisores é igual ao próprio número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.
-}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1..n-1], perfeito x]

perfeito :: Int -> Bool
perfeito n = n == sum (init (divisoresden n))

{-
6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.
-}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar lis1 lis2 = sum [a*b | (a,b) <- zip lis1 lis2]

{-
7. Usando List Comprehension escreva uma função, chamada primeirosPrimos, que devolva uma lista contendo os n primeiros números primos a partir do número 2.
-}
primeirosPrimos :: Int -> [Int]
primeirosPrimos n = [x | x <- [2..n], ehPrimo x]

ehPrimo :: Int -> Bool
ehPrimo n = if (divisoresden n == [1,n]) then True else False

{-
8. Usando List Comprehension escreva uma função, chamada paresOrdenados, que devolva uma lista de par ordenados contendo uma potência de 2 e uma potência de 3 até um determinado número dado. Observe que estes números podem ser bem grandes.
-}
paresOrdenados :: Int -> [(Int,Int)]
paresOrdenados n = [(x^2,x^3) | x <- [1..n] ]



main = do
  putStr "\nFunc. 1: entrada:20; resultado:"
  print (divisoresden 20)
  putStr "Func. 1: entrada:35; resultado:"
  print (divisoresden 35)
  putStr "Func. 1: entrada:100; resultado:"
  print (divisoresden 100)
  
  --putStr "Func. 2: entrada:100; resultado:"
  --print (contaCaractere "aviao")

  putStr "Func. 3: entrada:[1,2,3,-4]; resultado:"
  print (dobroNaoNegativo [1,2,3,-4])
  putStr "Func. 3: entrada:[-5,10,11]; resultado:"
  print (dobroNaoNegativo [-5,10,11])
  
  putStr "Func. 4: entrada:10; resultado:"
  print (pitagoras 10)
  
  putStr "Func. 5: entrada:50; resultado:"
  print (numerosPerfeitos 50)
  putStr "Func. 5: entrada:9000; resultado:"
  print (numerosPerfeitos 9000)
  
  putStr "Func. 6: entrada:[1,2,3] [1,2,3]; resultado:"
  print (produtoEscalar [1,2,3] [1,2,3])
  putStr "Func. 6: entrada:[1,2,3] [3,4,5]; resultado:"
  print (produtoEscalar [1,2,3] [3,4,5])
  putStr "Func. 6: entrada:[1,2,-3,5] [3,4,5,2]; resultado:"
  print (produtoEscalar [1,2,-3,5] [3,4,5,2])
  
  putStr "Func. 7: entrada:43; resultado:"
  print (primeirosPrimos 43)
  putStr "Func. 7: entrada:100; resultado:"
  print (primeirosPrimos 100)
  putStr "Func. 7: entrada:200; resultado:"
  print (primeirosPrimos 200)
  
  putStr "Func. 8: entrada:5; resultado:"
  print (paresOrdenados 5)





