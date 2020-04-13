#lang racket


(define (hoja? elem)
  (not (list? elem)))

(define (for-all? predicado lista)
  (or (null? lista)
      (and (predicado (car lista))
           (for-all? predicado (cdr lista)))))

;;;
;;;EJERCICIO 1
;;;


;;1A

(define lista-a '((a b) d (c (e) (f g) h)))



;;1C
;1C.1 Todos los elementos al cuadrado
;1C.2 el primer elemento que no es una hoja y el resto de la lista


;;;
;;;EJERCICIO 2
;;;

;;2A.a

(define (cuenta-pares lista)
  (if (null? lista) 0
      (if (hoja? lista)
          (if (even? lista) 1 0)
          (+ (cuenta-pares (car lista))
             (cuenta-pares (cdr lista))))))

(define (cuenta-pares-fos lista)
  (foldr + 0 (map (lambda (elem) (if (hoja? elem) (if (even? elem) 1 0) (cuenta-pares-fos elem))) lista )))

              
(define (todos-positivos lista)
  (if (null? lista) #t
      (if (hoja? lista) (positive? lista)
          (and (todos-positivos (car lista))
               (todos-positivos (cdr lista))))))


(define (todos-positivos-fos lista)
  (for-all? (lambda (elem) (if (hoja? elem) (positive? elem) (todos-positivos-fos elem))) lista))



;;;
;;;EJERCICIO 3
;;;

(define (cumplen-predicado funcion lista)
  (cond
    ((null? lista) '())
    ((hoja? lista) (if (funcion lista)(list lista) '()))
    (else (append (cumplen-predicado funcion (car lista)) (cumplen-predicado funcion (cdr lista))))))


(define (cumplen-predicado-fos funcion lista)
  (foldr append '() (map (lambda (elem) (if (hoja? elem) (if (funcion elem) (list elem) '()) (cumplen-predicado-fos funcion elem))) lista)))


(define (busca-mayores n lista-num)
  (cumplen-predicado (lambda (x) (> x n)) lista-num))

(define (empieza-por char lista-pal)
  (cumplen-predicado (lambda (x) (equal? char (string-ref (symbol->string x) 0))) lista-pal))


;;;
;;;EJERCICIO 4
;;;

(define (sustituye-elem elem-old elem-new lista)
  (if (null? lista) '()
      (if (hoja? lista)
          (if (equal? lista elem-old) elem-new lista)
          (cons (sustituye-elem elem-old elem-new (car lista))
                (sustituye-elem elem-old elem-new (cdr lista))))))


(define (diff-listas l1 l2)
  (if (null? l1) '()
      (if (hoja? l1)
          (if (not (equal? l1 l2)) (list (cons l1 l2)) '())
          (append (diff-listas (car l1) (car l2))
                  (diff-listas (cdr l1) (cdr l2))))))



;;;
;;;EJERCICIO 5
;;;

(define (mezclar lista1 lista2 n)
  (if (null? lista1) '()
      (if (hoja? lista1)
          (if (>= n 0) lista1 lista2)
          (cons (mezclar (car lista1) (car lista2) (- n 1))
                (mezclar (cdr lista1) (cdr lista2) n)))))
  

(define lista1 '(((a b) ((c))) (d) e))
(define lista2 '(((1 2) ((3))) (4) 5))


(define (incrementa-nivel pareja)
  (cons (car pareja) (+ (cdr pareja) 1)))

(define (mayor-pareja pareja1 pareja2)
  (if (null? pareja2) pareja1
      (if (> (cdr pareja1) (cdr pareja2)) pareja1 pareja2)))


(define (nivel-elemento lista)
  (if (null? lista) '()
      (if (hoja? lista) (cons lista 0)
          (mayor-pareja (incrementa-nivel (nivel-elemento (car lista)))
                        (nivel-elemento (cdr lista))))))
      


