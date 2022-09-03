import Data.Char

{-1. Escreva  uma  função  chamada  fatorialn  que  usando  o  
operador range  e  a  função  foldr devolva o fatorial de n. -}
fatorialn :: Int -> Int 
fatorialn n = foldr (*) 1 [1..n]

{-2. Usando a função map escreva uma função, chamada quadradoReal que 
recebe uma lista de números reais, positivos e negativos e devolva uma 
lista com o quadrado de cada um dos inteiros listados.-}
quadradoReal :: [Float] -> [Float]
quadradoReal lis = map (^2) lis

{-3. Usando a função map escreva uma função, comprimentoPalavras que
recebe uma lista de palavras e devolve uma lista com o comprimento de 
cada uma destas palavras. -}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras lis = map (length) lis

{-4. Usando a função filter escreva uma função, chamada 
MaiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja 
divisivel por 29.-}
maiorMultiploDe29 :: Int -> Int
maiorMultiploDe29 m = maximum (filter (\x -> x`mod`29==0) [0..m])

{-5. Usando  a  função  filter  escreva  uma  função,  chamada 
maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 
0 e 100000 que seja divisivel por este inteiro.-}
maiorMultiploDe :: Int -> Int
maiorMultiploDe x = maximum (filter (\a -> a`mod`x==0) [0..100000])

{-6. Usando Haskell e a função foldr defina uma função, chamada 
somaQuadrados que devolva a soma dos quadrados dos itens de uma lista 
de números naturais de comprimento n. De tal forma que: 
𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=1^2 +2^2 +3^2 +4^2...+𝑛^2. -}
somaQuadrados :: Int -> Int
somaQuadrados tamanho = foldr (+) 0 (listaQuadrados tamanho)

listaQuadrados :: Int -> [Int]
listaQuadrados tam = map (^2) [1..tam]

{-7. Usando Haskell e a função foldl defina uma função, chamada 
comprimento, que devolva o comprimento (cardinalidade) de uma lista 
dada.  -}
comprimento :: [Int] -> Int
comprimento lis = foldl (\x y -> x + 1) 0 lis

{-8. Esta é uma tarefa de pesquisa: você deve encontrar e executar 
exemplos em Haskell do uso das seguintes funções disponíveis no 
Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas 
funções você deverá encontrar, executar e testar no mínimo dois 
exemplos. -}
uncurried :: Int -> Int -> Int
uncurried = curry (\ (x,y) -> x^2+y^2)

potencia :: (Int, Int) -> Int
potencia = uncurry (^)

main = do
  putStr "\nFunc. 1: entrada:3; resultado:"
  print(fatorialn 3)
  putStr "Func. 1: entrada:4; resultado:"
  print(fatorialn 4)
  putStr "Func. 1: entrada:5; resultado:"
  print(fatorialn 5)

  putStr "Func. 2: entrada:[-2.2, -5, 10, 3.3]; resultado:"
  print(quadradoReal [-2.2, -5, 10, 3.3])
  
  putStr "Func. 3: entrada:[\"asa\",\"aviao\",\"haskell\"]; resultado:"
  print(comprimentoPalavras ["asa","aviao","haskell"])
  
  putStr "Func. 4: entrada:100000; resultado:"
  print(maiorMultiploDe29 100000)
  
  putStr "Func. 5: entrada:31; resultado:"
  print(maiorMultiploDe 31)
  putStr "Func. 5: entrada:43; resultado:"
  print(maiorMultiploDe 43)
  putStr "Func. 5: entrada:17; resultado:"
  print(maiorMultiploDe 17)
  putStr "Func. 5: entrada:19; resultado:"
  print(maiorMultiploDe 19)
  
  putStr "Func. 6: entrada:2; resultado:"
  print(somaQuadrados 2)
  putStr "Func. 6: entrada:3; resultado:"
  print(somaQuadrados 3)
  putStr "Func. 6: entrada:4; resultado:"
  print(somaQuadrados 4)
  putStr "Func. 6: entrada:5; resultado:"
  print(somaQuadrados 5)

  putStr "Func. 7: entrada:[1,2,3,4,5,6,7,8]; resultado:"
  print(comprimento [1,2,3,4,5,6,7,8])
  putStr "Func. 7: entrada:[1,2,3,4]; resultado:"
  print(comprimento [1,2,3,4])
  
  putStr "Func. 8: entrada:flip (/) 2 5; resultado:"
  print(flip (/) 2 5)
  putStr "Func. 8: entrada:flip (>) 21 15.0; resultado:"
  print(flip (>) 21 15.0)

  putStr "Func. 8: entrada:ord 'a'; resultado:"
  print(ord 'a')
  putStr "Func. 8: entrada:ord 'A'; resultado:"
  print(ord 'A')
  putStr "Func. 8: entrada:ord 'b'; resultado:"
  print(ord 'b')
  putStr "Func. 8: entrada:ord 'B'; resultado:"
  print(ord 'B')
  
  putStr "Func. 8: entrada:maximum [1,100,50]; resultado:"
  print(maximum [1,100,50])
  putStr "Func. 8: entrada:maximum [-5,-50,-100]; resultado:"
  print(maximum [-5,-50,-100])
  
  putStr "Func. 8: entrada:minimum [1,100,50]; resultado:"
  print(minimum [1,100,50])
  putStr "Func. 8: entrada:minimum [-5,-50,-100]; resultado:"
  print(minimum [-5,-50,-100])
  
  putStr "Func. 8: entrada:uncurried 4 5; resultado:"
  print(uncurried 4 5)
  putStr "Func. 8: entrada:uncurried 5 6; resultado:"
  print(uncurried 5 6)
  
  putStr "Func. 8: entrada:potencia (2,3); resultado:"
  print(potencia (2,3))
  putStr "Func. 8: entrada:potencia (4,3); resultado:"
  print(potencia (4,3))
  putStr "Func. 8: entrada:potencia (2,10); resultado:"
  print(potencia (2,10))
  




