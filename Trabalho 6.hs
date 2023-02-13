-- André Pacheco dos Santos

-- 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.
divisoresden :: Integer -> [Integer]
divisoresden alvo = [ x | x <- [ 1..(alvo-1) ], alvo `mod` x == 0 ]

-- 2. Usando List Comprehension escreva uma função, chamada contaCaractere, que conte a ocorrência de um caractere específico, em uma string dada.
contaCaractere :: String -> String -> Int
contaCaractere texto alvo = length [ x | x <- map (:[]) texto, x == alvo ]

-- 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [ x * 2 | x <- lista, x > 0 ]

-- 4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras alvo = [ (h, c1, c2) | h <- [1..alvo], c1 <- [1..alvo], c2 <- [1..alvo], ((c1*c1) + (c2*c2)) == (h*h), h > c1, h > c2, c1 > c2 ]

-- 5. Números perfeitos são aqueles cuja soma dos seus divisores é igual ao próprio número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.
numerosPerfeitos :: Integer -> [Integer]
numerosPerfeitos alvo = [ x | x <- [1..alvo], sum (divisoresden x) == x ]

-- 6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.
produtoEscalar :: [Integer] -> [Integer] -> Integer
produtoEscalar l1 l2 = sum [ fst x * snd x | x <- zip l1 l2 ]

-- 7. Usando List Comprehension escreva uma função, chamada primeirosPrimos, que devolva uma lista contendo os n primeiros números primos a partir do número 2.
primeirosPrimos :: Integer -> [Integer]
primeirosPrimos alvo = [ x | x <- [ 2..alvo ], length [ y | y <- [ 2..(x-1) ], x `mod` y == 0 ] == 0 ]

-- 8. Usando List Comprehension escreva uma função, chamada paresOrdenados, que devolva uma lista de par ordenados contendo uma potência de 2 e uma potência de 3 até um determinado número dado. Observe que estes números podem ser bem grandes.
paresOrdenados :: Int -> [(Float, Float)]
paresOrdenados alvo = [(1.0, 1.0)] ++ [ (2^x, 3^x) | x <- [1..alvo] ]

main = do
  putStrLn $ "Func1: entrada:20; resultado:" ++ show(divisoresden 20)
  putStrLn $ "Func2: entrada:arara a; resultado:" ++ show(contaCaractere "arara" "a")
  putStrLn $ "Func3: entrada:[10, -5, 50]; resultado:" ++ show(dobroNaoNegativo [10, -5, 50])
  putStrLn $ "Func4: entrada:20; resultado:" ++ show(pitagoras 20)
  putStrLn $ "Func5: entrada:500; resultado:" ++ show(numerosPerfeitos 500)
  putStrLn $ "Func6: entrada:[2, 2, 2] [2, 2, 2]; resultado:" ++ show(produtoEscalar [2, 2, 2] [2, 2, 2])
  putStrLn $ "Func7: entrada:15; resultado:" ++ show(primeirosPrimos 15)
  putStrLn $ "Func8: entrada:2; resultado:" ++ show(paresOrdenados 2)
