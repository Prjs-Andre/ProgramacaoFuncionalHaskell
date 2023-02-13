; André Pacheco dos Santos

; 1. Na  aula  disponível  em: https://replit.com/@frankalcantara/ClojureAula2?v=1 foram destacadas diversas funções (expressões), como funções de primeira ordem, disponíveis em Clojure. Sua primeira tarefa será descrever cada uma destas funções e apresentar dois exemplos de uso de cada uma delas. Lembre-se os exemplos precisam ser utilizados de forma que o resutado da função possa ser visto no terminal.

; assoc: quando aplicada a um mapa retorna um novo mapa com as novas chaves, quando aplicada a um vetor coloca na posição indica o novo valor.
(println "Função assoc ex 1: entrada: [3 2 3] 0 7; resultado: "(assoc [3 2 3] 0 7))
(println "Função assoc ex 2: entrada: [4 1 2] 3 7; resultado: "(assoc [4 1 2] 3 7))

; dissoc: remove determinadas chaves de um mapa. 
(println "Função dissoc ex 1: entrada: {:a 1 :b 1 :c 1} :b; resultado: "(dissoc {:a 1 :b 1 :c 1} :b))
(println "Função dissoc ex 2: entrada: {:a 1 :b 1 :c 1} :c :b; resultado: "(dissoc {:a 1 :b 1 :c 1} :c :b))

; range: cria uma sequencia de valores de x a x. Inicio padrão é 0.
(println "Função range ex 1: entrada: 5; resultado: "(range 5))
(println "Função range ex 2: entrada: 15 20; resultado: "(range 15 20))

; map: retorna uma sequencia que consiste no resultado da aplicação de f ao conjunto dos primeiros itens de cada coluna, seguido pela aplicação de f ao conjunto de segundos itens em cada coluna.
(println "Função map ex 1: entrada: [1 2 3] [4 5 6]; resultado: "(map + [1 2 3] [4 5 6]))
(println "Função map ex 2: entrada: [2 3 4] [4 5 6]; resultado: "(map * [2 3 4] [4 5 6]))

; inc: retorna um número maior do que um número dado.
(println "Função inc ex 1: entrada: 12; resultado: "(inc 12))
(println "Função inc ex 2: entrada: [2 2 4]; resultado: "(map inc [2 2 4]))

; filter: retorna uma sequencia do item passado como parametro para os quais a expressão for verdadeira. 
(println "Função filter ex 1: entrada: 3; resultado: " (filter even? (range 3)))
(println "Função filter ex 2: entrada: 10; resultado: " (filter odd? (range 10)))

; into: retorna uma saida onde ocorreu o uso de todos os itens passados no parametro.
(println "Função into ex 1: entrada: 3 2, 4 6}; resultado: "(into [] {3 2, 4 6}))
(println "Função into ex 2: entrada: [2 2 2] '(3 3 3); resultado: "(into [2 2 2] '(3 3 3)))

; nth: retorna o valor no indíce passado como parametro.
(println "Função nth ex 1: entrada: [3, 2, 4, 3] 2; resultado: " (nth [3, 2, 4, 3] 2))
(println "Função nth ex 2: entrada: [1, 2, 3, 4] 3; resultado: " (nth [1, 2, 3, 4] 3))

; conj: adiciona valores passados como parametro a uma coleção.
(println "Função conj ex 1: entrada: [1 2 3] 4; resultado: " (conj [1 2 3] 4))
(println "Função conj ex 2: entrada: [1 2] 3 4; resultado: " (conj [1 2] 3 4))

; sort: ordena uma coleção. Por padrão de forma crescente.
(println "Função sort ex 1: entrada: [3 1 2 4]; resultado: " (sort [3 1 2 4]))
(println "Função sort ex 2: entrada: [d a b c j]; resultado: " (sort ["d" "a" "b" "c" "j"]))

; partition-by: aplica f a uma coleção, sempre que retornar um novo valor divide a coleção.
(println "Função partition-by ex 1: entrada: [2 2 3 3 4 5 5]; resultado: " (partition-by odd? [2 2 3 3 4 5 5]))
(println "Função partition-by ex 2: entrada: [1 2 2 2 3 3 3]; resultado: " (partition-by even? [1 2 2 2 3 3 3]))

; empty: retorna true caso a coleção passada como parametro esteja vazia e false para caso não esteja. 
(println "Função empty ex 1: entrada: (); resultado: " (empty? ()))
(println "Função empty ex 2: entrada: '(2); resultado: " (empty? '(2)))

; count: retorna o retorna o numero de itens de uma coleção.
(println "Função count ex 1: entrada: []; resultado: " (count []))
(println "Função count ex 2: entrada: [1 1 1]; resultado: " (count [1 1 1]))

; char: retorna o char no índice fornecido.
(println "Função char ex 1: entrada: 98; resultado: " (char 98))
(println "Função char ex 2: entrada: [66 65 68 67]; resultado: " (map char [66 65 68 67]))

; 2. Utilizando a linguagem Clojure, crie uma função chamada ehPrimo que receba um inteiro e devolva True caso este inteiro seja verdadeiro e False caso contrário.
(defn ehPrimo [arg] (
  loop [valor 1 divisores []]
    (if (= valor (+ arg 1)) 
      (if (= (count divisores) 2) true false)
       (recur (inc valor) (if (zero? (mod arg valor)) (conj divisores valor) divisores))
    )
  )
)

(println "Função 2: entrada: 53; resultado: " (ehPrimo 53))

; 3. Utilizando a linguagem Clojure, crie uma função chamada fatoresPrimos que receba um inteiro e devolva uma lista dos seus fatores primos. Decomposição em fatores primos é uma tarefa fundamental da aritmética.
(defn primoAux [valor divisor]
  (if (= valor divisor) true 
    (if (= (mod valor divisor) 0) false 
      (primoAux valor (+ divisor 1))
    )
  )
)

(defn proximoDivisor [arg divisor]
  (if (false? (primoAux divisor 2)) 
    (proximoDivisor arg (+ divisor 1))
    (if (= (mod arg divisor) 0)
      divisor (proximoDivisor arg (+ divisor 1))
    )
  )
)

(defn fatoresPrimos [arg]
  (if (true? (primoAux arg 2)) 
    [arg] (concat [(proximoDivisor arg 2)] (fatoresPrimos (/ arg (proximoDivisor arg 2))))
  )
)

(println "Função 3: entrada: 70; resultado: " (fatoresPrimos 70))

; 4. Utilizando a linguagem Clojure, crie uma função chamada todosPrimos que receba dois valores inteiros e devolve todos os números primos que existam entre estes dois valores.
(defn todosPrimos [arg1 arg2] (
  loop [valores1 (range arg1 (+ arg2 1)) valores2 []]
    (if (empty? (rest valores1)) 
      valores2 
      (recur (rest valores1) 
      (if (ehPrimo (nth valores1 0)) (conj valores2 (nth valores1 0)) valores2)
      )
    )
  )
)

(println "Função 4: entrada: 1 105; resultado: " (todosPrimos 10 100))
