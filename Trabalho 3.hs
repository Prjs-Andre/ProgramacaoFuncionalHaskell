-- André Pacheco dos Santos

-- 1. Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando Haskell.
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo x = fibo(x-1) + fibo(x-2)

-- 2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma função para o cálculo do MDC entre dois números inteiros positivos, usando o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
mdc :: Integer -> Integer -> Integer
mdc x y
  | y <= 0 = x
  | otherwise = mdc y (x `mod` y)

-- 3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e recursividade.
soma :: Integer -> Integer
soma 0 = 0
soma x = x `mod` 10 + soma(x `div` 10)

-- 4. Escreva uma função que devolva a soma de todos os números menores que 10000 que sejam múltiplos de 3 ou 5.
somaMenoresDez :: Integer -> Integer -> Integer
somaMenoresDez a b
  | a == 10000 = b
  | a `mod` 3 == 0 || a `mod` 5 == 0 = somaMenoresDez (a+1) (b+a)
  | otherwise = somaMenoresDez (a+1) b

-- 5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
diferencaListaAux :: [Integer] -> Integer -> Integer
diferencaListaAux a b
  | (tail a) == [] = head a^b
  | otherwise = (head a)^b + diferencaListaAux (tail a) b

diferencaLista :: [Integer] -> Integer
diferencaLista x = diferencaListaAux x 2 - diferencaListaAux x 1^2

-- 6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado.
crivoAux :: Integer -> Integer -> Integer
crivoAux n d
  | n <= 2 && n == 2 = n
  | n <= 2 = 0
  | n `mod` d == 0 = 0
  | d * d > n = n
  | otherwise = crivoAux n (d+1)

crivo :: Integer -> [Integer] -> [Integer]
crivo x y
  | x <= 1 = filter (/= 0) y
  | otherwise = crivo (x-1) ([crivoAux(x-1) 2] ++ y)

-- 7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado.
lucasAux :: Integer -> Integer
lucasAux 0 = 2
lucasAux 1 = 1
lucasAux a = lucasAux(a-1) + lucasAux(a-2)

lucas :: Integer -> [Integer] -> [Integer]
lucas x y
  | x == 0 && y == [] = [2]
  | x == 1 && y == [] = [2, 1]
  | x == 0 = y
  | otherwise = lucas (x-1) ([lucasAux(x-1)] ++ y)

-- 8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1].
aoContrario :: [Integer] -> [Integer]
aoContrario [] = []
aoContrario (x:xs) = aoContrario(xs) ++ [x]

-- 9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação.
somaRecursiva :: Integer -> Integer -> Integer
somaRecursiva x y
  | y == 0 = 0
  | y < 0 = -1
  | otherwise = x + somaRecursiva x (y-1)

-- 10. Escreva uma função chamada comprimento que receba uma lista de inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista.
comprimento :: [Integer] -> Integer
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento(xs)

main = do
  putStrLn $ "Func1: entrada:10; resultado:" ++ show(fibo 10)
  putStrLn $ "Func2: entrada:10 25; resultado:" ++ show(mdc 10 25)
  putStrLn $ "Func3: entrada:1234; resultado:" ++ show(soma 1234)
  putStrLn $ "Func4: entrada: 0 0; resultado:" ++ show(somaMenoresDez 0 0)
  putStrLn $ "Func5: entrada:[1,2,3,4]; resultado:" ++ show(diferencaLista [2,2,2])
  putStrLn $ "Func6: entrada:100; resultado:" ++ show(crivo 100 [])
  putStrLn $ "Func7: entrada:2 []; resultado:" ++ show(lucas 10 [])
  putStrLn $ "Func8: entrada:[1,2,3]; resultado:" ++ show(aoContrario [1,2,3])
  putStrLn $ "Func9: entrada:2 5; resultado:" ++ show(somaRecursiva 2 5)
  putStrLn $ "Func10: entrada:[1,2,3,4,5,5]; resultado:" ++ show(comprimento [1,2,3,4,5,5])
