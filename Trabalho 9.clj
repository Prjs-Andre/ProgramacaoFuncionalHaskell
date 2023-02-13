; André Pacheco dos Santos

; 1. Utilizando  a  linguagem  Clojure,  crie  uma  função  chamada  ultimo  que  receba  uma  lista  e devolva o último elemento desta lista sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.
(defn ultimo [arg] 
  (nth arg (- (count arg) 1))
)

(println "Func 1. Entrada: [2 1 6 8 9] 2. Saida: " (ultimo [2 1 6 8 9]))

; 2. Utilizando a linguagem Clojure, crie uma função chamada penultimo que receba uma lista e  devolva  o  penúltimo  elemento  desta  lista  usar as  funções  já  prontas  e disponíveis para esta mesma finalidade na linguagem Clojure.
(defn penultimo [arg] 
  (nth arg (- (- (count arg) 1) 1))
)

(println "Func 2. Entrada: [2 1 6 8 9] 2. Saida: " (penultimo [2 1 6 8 9]))

; 3. Utilizando a linguagem Clojure, crie uma função chamada elementoN que receba uma lista e um inteiro N e devolva o  elemento que  está na  posição N desta lista usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.
(defn elementoN [lista n]
  (loop [i n listax lista]
    (if (== i 0)
      (first listax)
      (recur (- i 1) (rest listax))
    )
  )
)

(println "Func 3. Entrada: [2 1 6 8 9] 2. Saida: " (elementoN [2 1 6 8 9] 2))

; 4. Utilizando  a  linguagem Clojure,  crie  uma função  chamada  inverso  que  receba uma  lista  e devolva esta lista com as posições dos elementos invertidas. Por exemplo recebe [1,2,3] e devolve [3,2,1]. Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.
(defn inverso [arg]
  (loop [arg arg acc (empty arg)]
    (if (empty? arg)
      acc (recur (rest arg) (cons (first arg) acc))
    )
  )
)

(println "Func 4. Entrada: [5 4 3 2 1]. Saida: " (inverso [5 4 3 2 1]) )

; 5. Utilizando a  linguagem Clojure, crie uma função chamada  mdc que receba  dois inteiros e devolve o mínimo divisor comum entre eles.  Sem usar as funções já prontas e disponíveis para esta mesma finalidade na linguagem Clojure.  

(defn mdc [arg1 arg2]
  (loop [alvo (max arg1 arg2) ]
    (if (or 
         (identical? alvo 1) 
         (and (zero? (mod arg1 alvo)) (zero? (mod arg2 alvo))) 
        )
        alvo (recur (dec alvo))
    )
  )
)

(println "Func 5. Entrada: 8 22. Saida: " (mdc 8 22))
