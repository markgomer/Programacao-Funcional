-- Marco Aurélio Silva de Souza Júnior

{-
1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada. 
-}
soma1 :: Int -> Int
soma1 x = x + 1

-- 2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. 
zero :: a -> Int
zero x = 0

-- 3. Escreva uma função chamada treco que receba três valores  em ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. 
treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z

-- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros. 
resto :: Int -> Int -> Int 
resto x y = x `mod` y

-- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários. 
precoMaior :: Double -> Double -> Double -> Double -> Double
precoMaior x y z = maior (maior (maior x y) z)

maior :: Double -> Double -> Double
maior a b = if a >= b then a else b


-- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar
impar :: Int -> Int -> Bool
impar x y = multiplicar x y `mod` 2 == 1

multiplicar :: Int -> Int -> Int
multiplicar a b = a * b

-- Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
somaPar :: (Int,Int) -> Int
somaPar (x,y) = x + y

-- Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥^2 + 𝑦/2 + 𝑧.
equacao :: Double -> Double -> Double -> Double
equacao x y z = x^2 + y/2 + z

{- Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link: 
Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos (cuidadospelavida.com.br). 
Observe que este diagnóstico é meramente estatístico e não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. 
Todo e qualquer diagnóstico deve ser feito por um profissional médico. 
-}
diagnostico :: Double -> Double -> String
diagnostico peso alturaMetro
  | imc peso alturaMetro <= 17 = "Muito abaixo do peso"
  | imc peso alturaMetro <= 18.49 = "Abaixo do peso"
  | imc peso alturaMetro <= 24.99 = "Peso normal"
  | imc peso alturaMetro <= 29.99 = "Sobrepeso"
  | imc peso alturaMetro <= 34.99 = "Obesidade leve"
  | imc peso alturaMetro <= 39.99 = "Obesidade severa"
  | otherwise = "Obesidade morbida"

imc :: Double -> Double -> Double
imc peso alturaEmMetro = peso / alturaEmMetro^2

{-
Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o 
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra: 
𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4
𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100
𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400
1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
-}
bissexto :: Int -> Bool
bissexto ano = 
  ano `mod` 4 == 0 && 
  ano `mod` 100 /= 0 && 
  ano `mod` 400 /= 0
  


main = do
  putStr ("Func. 1: entrada:1; resultado:")
  print (soma1 1)
  
  putStr ("Func. 2: entrada:99; resultado:")
  print (zero 99)
  putStr ("Func. 2: entrada:55.1234; resultado:")
  print (zero 55.1234)
  
  putStr ("Func. 3: entrada:3.5 1.5 3; resultado:")
  print (treco 3.5 1.5 3)
  
  putStr ("Func. 4: entrada:10 3; resultado:")
  print (resto 10 3)

  putStr ("Func. 5: entrada:2.5 3.5 1.2 0.5; resultado:")
  print (precoMaior 2.5 3.5 1.2 0.5)
  putStr ("Func. 5: entrada:2.5 3.5 1.2 4.5; resultado:")
  print (precoMaior 2.5 3.5 1.2 4.5)
  --print (precoMaior 2.5 3.5)

  putStr ("Func. 6: entrada:2 4; resultado:")
  print (impar 2 4)
  putStr ("Func. 6: entrada:3 3; resultado:")
  print (impar 3 3)

  putStr ("Func. s/n: entrada:5 5; resultado:")
  print (somaPar (5, 5))
  putStr ("Func. s/n: entrada:12 13; resultado:")
  print (somaPar (12, 13))

  putStr ("Func. 7: entrada:1 1 1; resultado:")
  print (equacao 1 1 1)
  putStr ("Func. 7: entrada:2 4 3.7; resultado:")
  print (equacao 2 4 3.7)

  putStr ("Func. 8: entrada:70 1.80; resultado:")
  print (diagnostico 70 1.80)
  putStr ("Func. 8: entrada:50 1.80; resultado:")
  print (diagnostico 50 1.80)
  putStr ("Func. 8: entrada:95 1.80; resultado:")
  print (diagnostico 95 1.80)
  putStr ("Func. 8: entrada:50 1.60; resultado:")
  print (diagnostico 50 1.60)
  putStr ("Func. 8: entrada:120 1.60; resultado:")
  print (diagnostico 120 1.60)
  putStr ("Func. 8: entrada:45 1.50; resultado:")
  print (diagnostico 45 1.50)

  putStr ("Func. 9: entrada:1997; resultado:")
  print (bissexto 1997)
  putStr ("Func. 9: entrada:1996; resultado:")
  print (bissexto 1996)
  putStr ("Func. 9: entrada:1990; resultado:")
  print (bissexto 1900)
  putStr ("Func. 9: entrada:2000; resultado:")
  print (bissexto 2000)
  putStr ("Func. 9: entrada:2004; resultado:")
  print (bissexto 2004)

  
