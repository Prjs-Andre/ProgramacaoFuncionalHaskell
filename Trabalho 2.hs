-- André Pacheco dos Santos

-- 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.
soma1 :: Integer -> Integer
soma1 x = x + 1

-- 2. Escreva uma função chamada sempre que, não importando o valor  de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
sempre :: a -> Integer
sempre x = 0

-- 3. Escreva uma função chamada treco que receba três valores em  ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
treco :: Float -> Float -> Float -> Float
treco x y z = (x + y) * z

-- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
resto :: Integer -> Integer -> Integer
resto x y = x `rem` y

-- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.
precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior w x y z = maximum[w, x, y, z]

-- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.
impar :: Integer -> Integer -> Bool
impar x y = odd(x * y)

-- Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
somaPar :: (Int, Int) -> Int
somaPar x = fst(x) + snd(x)

-- 7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥2 +𝑦/2 +𝑧.
equacao :: Double -> Double -> Double -> Double
equacao x y z = (x * x) + (y / 2) + z

-- 8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela que pode ser encontrada no link: Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos (cuidadospelavida.com.br). Observe que este diagnóstico é meramente  estatístico e não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.
diagnostico :: Double -> String
diagnostico x 
  | x < 17 = "Muito abaixo do peso"
  | x >= 17 && x < 18.5 = "Abaixo do peso"
  | x >= 18.5 && x < 25 = "Peso normal"
  | x >= 25 && x < 30 = "Sobrepeso"
  | x >= 30 && x < 35 = "Obesidade Leve"
  | x >= 35 && x < 40 = "Obesidade severa"
  | otherwise = "Obesidade morbida"

-- 9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra: 𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
bissexto :: Integer -> Bool
bissexto x = if x `rem` 4 == 0 || x `rem` 400 == 0 && x `rem` 100 /= 0 then True else False

main = do
  putStrLn $ "soma1: entrada:0; resultado:" ++ show(soma1 (-1))
  putStrLn $ "sempre: entrada:45; resultado:" ++ show(sempre 45)
  putStrLn $ "treco: entrada:1, 2, 3; resultado:" ++ show(treco 1 2 3)
  putStrLn $ "resto: entrada:7, 3; resultado:" ++ show(resto 7 3)
  putStrLn $ "precoMaior: entrada:212.54, 2.5, 3.99, 55.0; resultado:" ++ show(precoMaior 212.54 2.5 3.99 55.0)
  putStrLn $ "impar: entrada:2, 3; resultado:" ++ show(impar 2 3)
  putStrLn $ "somaPar: entrada:2, 1; resultado:" ++ show(somaPar (2, 1))
  putStrLn $ "equacao: entrada:2, 2, 2; resultado:" ++ show(equacao 2 2 2)
  putStrLn $ "diagnostico: entrada:70, 1.75; resultado:" ++ show(diagnostico 17)
  putStrLn $ "bissexto: entrada:2001; resultado:" ++ show(bissexto 2001)
