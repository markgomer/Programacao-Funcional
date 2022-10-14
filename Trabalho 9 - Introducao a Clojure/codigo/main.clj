;; Marco Aurélio Silva de Souza Júnior


;; 1. Utilizando a linguagem Clojure, crie uma função chamada ultimo que
;; receba uma lista e devolva o último elemento desta lista sem usar as 
;; funções já prontas e disponíveis para esta mesma finalidade na 
;; linguagem Clojure.
(defn ultimo 
  "Retorna ultimo elemento de uma lista" [lista]
  (nth lista 
    (- (count lista) 1) ) 
)
(print "Func.1: entrada:1 2 3 4 5; saida:")
(println (ultimo (list 1 2 3 4 5) ) )

(comment
  2. Utilizando a linguagem Clojure, crie uma função chamada penultimo que 
  receba uma lista e devolva o penúltimo elemento desta lista usar as 
  funções já prontas e disponíveis para esta mesma finalidade na linguagem 
  Clojure. )
(defn penultimo
  "Retorna penultimo elemento da lista" [lista]
  (nth lista (- (count lista) 2) ) 
)
(print "Func.2: entrada:[1 2 3 4 5]; saida:")
(println (penultimo [1 2 3 4 5]))


(comment
 3. Utilizando a linguagem Clojure, crie uma função chamada elementoN que
 receba uma lista e um inteiro N e devolva o elemento que está na posição N
 desta lista usar as funções já prontas e disponíveis para esta mesma
 finalidade na linguagem Clojure. )
(defn elementoN
  "Retorna o enesimo elemento da lista" [lista N]
  (nth lista N) 
)
(print "Func.3: entrada:[1 2 3 4 5] 2; saida:")
(println (elementoN [1 2 3 4 5] 2))

(comment 
 4. Utilizando a linguagem Clojure, crie uma função chamada inverso que
 receba uma lista e devolva esta lista com as posições dos elementos 
 invertidas. Por exemplo recebe [1,2,3] edevolve [3,2,1]. Sem usar as 
 funções já prontas e disponíveis para esta mesma finalidade na linguagem 
 Clojure. )
(defn inverso
  "Recebe uma lista e inverte" [lista]
  (if (empty? lista)
    () ; se lista estiver vazia, retorna lista vazia
    (concat ; junta duas listas
      (inverso (rest lista)) ; chamada recursiva com tail da lista
      (list (elementoN lista 0)) ; transforma 1o. elem. em lista para concatenar ao final da lista
    )
  )
)
(print "Func.4: entrada:[1 2 3 4 5]; saida:")
(println (inverso [1 2 3 4 5]))


(comment 
 5. Utilizando a linguagem Clojure, crie uma função chamada mdc que receba 
 dois inteiros e devolve o máximo divisor comum entre eles. Sem usar as 
 funções já prontas e disponíveis para esta mesma finalidade na linguagem 
 Clojure. )
(defn mdc 
  "Calcula máximo divisor comum entre dois inteiros" [x y]
  (if (== y 0) 
    x ; se y = 0, retorna x
    (if (>= x y) 
      (mdc y (mod x y)) 
      (mdc x (mod y x))
    )
  )
)
(print "Func.5: entrada:15 10; saida:")
(println (mdc 15 10))
(print "Func.5: entrada:49 28; saida:")
(println (mdc 49 28))
(print "Func.5: entrada:21 27; saida:")
(println (mdc 21 27))
