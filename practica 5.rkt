#lang racket
(require graphics/turtles)

;;;
;;;EJERCICIO 1
;;;

;;1A
(define (concat-iter lista result)
  (if (null? lista)
      result
      (concat-iter (cdr lista) (string-append result (car lista)))))

(define (concat lista)
  (concat-iter lista ""))



;;1B
(define (min-max-iter lista result)
  (cond
    ((null? lista) result)
    ((< (car lista) (car result)) (min-max-iter (cdr lista) (cons (car lista) (cdr result))))
    ((> (car lista) (cdr result)) (min-max-iter (cdr lista) (cons (car result) (car lista))))
    (else (min-max-iter (cdr lista) result))))

(define (min-max lista)
  (min-max-iter lista (cons 2 2)))




;;;
;;;EJERCICIO 2
;;;

;;2A
(define (expande-pareja-iter pareja result)
  (if (= (cdr pareja) 0) result
      (expande-pareja-iter (cons (car pareja) (- (cdr pareja) 1)) (cons (car pareja) result))))

(define (expande-pareja pareja)
  (expande-pareja-iter pareja '()))


(define (expande-parejas-iter lista-parejas result)
  (if (null? lista-parejas) result
      (expande-parejas-iter (cdr lista-parejas) (append result (expande-pareja (car lista-parejas))))))

(define (expande-parejas . lista-parejas)
  (expande-parejas-iter lista-parejas '()))



;;2B
(define (rotar k lista)
  (if (= k 0) lista
      (rotar (- k 1) (append (cdr lista) (list (car lista))))))


;;;
;;;EJERCICIO 3
;;;

;3A
(define (mi-foldl funcion resultado base)
  (if (null? base) resultado
      (mi-foldl funcion (funcion (car base) resultado) (cdr base))))


;;3B
(define (prefijo-lista? lista1 lista2)
  (cond
    ((null? lista1) #t)
    ((not (equal? (car lista1) (car lista2))) #f)
    (else (prefijo-lista? (cdr lista1) (cdr lista2)))))



;;;TORTUGA

;;5A

(turtles #t)
(define (koch nivel trazo)  
  (if (= nivel 0)
      (draw trazo)
      (begin
        (koch (- nivel 1) (/ trazo 3))
        (turn 60)
        (koch (- nivel 1) (/ trazo 3))
        (turn -120)
        (koch (- nivel 1) (/ trazo 3))
        (turn 60)
        (koch (- nivel 1) (/ trazo 3)))))


(define (copo-nieve nivel trazo)
  (if (> nivel 0)
      (begin
        (koch nivel trazo)
        (turn -120)
        (koch nivel trazo)
        (turn -120)
        (koch nivel trazo)
        (turn -120))
        (move 0)))


(define (cuadrado trazo)
  (begin
    (draw trazo)
    (turn 90)
    (draw trazo)
    (turn 90)
    (draw trazo)
    (turn 90)
    (draw trazo)
    (turn 90)))


(define (alfombra-sierpinski tamaño)
  (if (> tamaño 20)
      (begin
        (alfombra-sierpinski (/ tamaño 3))
        (move (/ tamaño 3))
        (alfombra-sierpinski (/ tamaño 3))
        (move (/ tamaño 3))
        (alfombra-sierpinski (/ tamaño 3))
        (turn 90) (move (/ tamaño 3)) (turn -90)
        (alfombra-sierpinski (/ tamaño 3))
        (turn 90) (move (/ tamaño 3)) (turn -90)
        (alfombra-sierpinski (/ tamaño 3))
        (turn 180) (move (/ tamaño 3)) (turn -180)
        (alfombra-sierpinski (/ tamaño 3))
        (turn 180) (move (/ tamaño 3)) (turn -180)
        (alfombra-sierpinski (/ tamaño 3))
        (turn -90) (move (/ tamaño 3)) (turn 90)
        (alfombra-sierpinski (/ tamaño 3))
        (turn -90) (move (/ tamaño 3)) (turn 90))
       (cuadrado tamaño) ) )
        
        









