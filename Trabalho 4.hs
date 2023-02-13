-- André Pacheco dos Santos

-- 1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr devolva o fatorial de n.
fatorialn :: Integer -> Integer
fatorialn x
  | x <= 1 = 1
  | otherwise = foldl (*) 1 [x, x-1..1]

-- 2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados
quadradoReal :: [Integer] -> [Integer]
quadradoReal x = map (^ 2) x

-- 3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras x = map (length) x

-- 4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.
aoContrario :: [Integer] -> [Integer]
aoContrario [] = []
aoContrario (x:xs) = aoContrario xs ++ [x]

maiorMultiploDe29 :: Integer
maiorMultiploDe29 = head (filter p (aoContrario [0..100000]))
  where p x = x `mod` 29 == 0

-- 5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
maiorMultiploDe :: Integer -> Integer
maiorMultiploDe x = head (filter p (aoContrario [0..100000]))
  where p y = y `mod` x == 0

-- 6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 1^2 + 2^2 + 3^2 + 4^2 . . . +𝑛^2.
somaQuadrados :: Integer -> Integer
somaQuadrados x = foldr ((+) . (^ 2)) 0 [1..x]

-- 7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.
comprimento :: [Integer] -> Integer
comprimento lista = foldl (\contador _ -> contador+1) 0 lista

-- 8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.

main = do
  putStrLn $ "Func1: entrada:5; resultado:" ++ show (fatorialn 3)
  putStrLn $ "Func2: entrada:[5,-2,11]; resultado:" ++ show (quadradoReal [5,-2,11])
  putStrLn $ "Func3: entrada:['andre','haskell','hello world','teste']; resultado:" ++ show (comprimentoPalavras ["andre","haskell","hello world","teste"])
  putStrLn $ "Func4: entrada:; resultado:" ++ show maiorMultiploDe29
  putStrLn $ "Func5: entrada:12; resultado:" ++ show (maiorMultiploDe 12)
  putStrLn $ "Func6: entrada:5; resultado:" ++ show (somaQuadrados 5)
  putStrLn $ "Func7: entrada:[2,2,2,2]; resultado:" ++ show (comprimento [2,2,2,2])
  
