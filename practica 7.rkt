#lang racket

;--------------------------------------;
; Implementación FOS exist? y for-all? ;
;--------------------------------------;

(define (exists? predicado lista)
  (and (not (null? lista))
       (or (predicado (car lista))
           (exists? predicado (cdr lista)))))

(define (for-all? predicado lista)
  (or (null? lista)
      (and (predicado (car lista))
           (for-all? predicado (cdr lista)))))

;---------------------------------------;
; Barrera de abstracción arbol genérico ;
;---------------------------------------;

(define (dato-arbol arbol) 
    (car arbol))

(define (hijos-arbol arbol) 
    (cdr arbol))

(define (hoja-arbol? arbol) 
   (null? (hijos-arbol arbol)))

(define (nuevo-arbol dato lista-arboles)
   (cons dato lista-arboles))

;--------------------------------------;
; Barrera de abstracción arbol binario ;
;--------------------------------------;

(define (dato-arbolb arbol)
   (car arbol))

(define (hijo-izq-arbolb arbol)
   (cadr arbol))

(define (hijo-der-arbolb arbol)
   (caddr arbol))

(define arbolb-vacio '())

(define (vacio-arbolb? arbol)
   (equal? arbol arbolb-vacio))

(define (hoja-arbolb? arbol)
   (and (vacio-arbolb? (hijo-izq-arbolb arbol))
        (vacio-arbolb? (hijo-der-arbolb arbol))))

(define (nuevo-arbolb dato hijo-izq hijo-der)
    (list dato hijo-izq hijo-der))



;;;::::::::::::
;;;EJERCICIO 1:
;;;::::::::::::

;;
;;1A
;;

(define arbol '(15 (4 (2) (3)) (8 (6)) (12 (9) (10) (11))))

;La suma de los datos del primer hijo
;La suma de todos los hijos

;A.3

; '(9 14 42)
; (+ 42 15) (+ 14 57) (+ 9 71)



;;
;;1B.1
;;

(define arbolb '(40 (23 (5 () ()) (32 (29 () ()) ())) (45 () (56 () ()))))


;;;::::::::::::
;;;EJERCICIO 2:
;;;::::::::::::

;;
;;2.A
;;

(define (to-string-bosque bosque)
  (if (null? bosque) ""
      (string-append (to-string-arbol (car bosque))
                     (to-string-bosque (cdr bosque)))))

(define (to-string-arbol arbol)
  (string-append (symbol->string (dato-arbol arbol))
                 (to-string-bosque (hijos-arbol arbol))))


(define (to-string-arbol-fos arbol)
  (foldr string-append  "" (cons (symbol->string (dato-arbol arbol)) (map to-string-arbol-fos (hijos-arbol arbol)))))


;;
;;2.B
;;

(define (veces-bosque dato bosque)
  (if (null? bosque)
      0
      (+ (veces-arbol dato (car bosque))
         (veces-bosque dato (cdr bosque)))))

(define (veces-arbol dato arbol)
  (if (equal? (dato-arbol arbol) dato)
      (+ 1 (veces-bosque dato (hijos-arbol arbol)))
      (veces-bosque dato (hijos-arbol arbol))))


(define (veces-arbol-fos dato arbol)
  (foldr + (if (equal? (dato-arbol arbol) dato) 1 0) (map (lambda (elem)
                                                            (veces-arbol-fos dato elem)) (hijos-arbol arbol))))



;;;::::::::::::
;;;EJERCICIO 3:
;;;::::::::::::

(define (bosque-hojas-cumplen pred bosque)
  (if (null? bosque)
      '()
      (append (hojas-cumplen pred (car bosque))
              (bosque-hojas-cumplen pred (cdr bosque)))))

(define (hojas-cumplen pred arbol)
  (if (hoja-arbol? arbol)
      (if (pred (dato-arbol arbol))
        (list (dato-arbol arbol))
        '())

      (bosque-hojas-cumplen pred (hijos-arbol arbol))))



(define (hojas-cumplen-fos pred arbol)
  (foldr append (if (hoja-arbol? arbol)
                    (if (pred (dato-arbol arbol))
                    (list (dato-arbol arbol))
                    '())
                    '())
         (map (lambda (elem) (hojas-cumplen-fos pred elem)) (hijos-arbol arbol))))

(define arbol1 '(10 (2) (12 (4) (2)) (10 (5))))
(define arbol2 '(10 (2) (12 (4) (2)) (10 (6))))
;:(hojas-cumplen even? arbol1) ; ⇒ '(2 4 2)
;:(hojas-cumplen even? arbol2) ; ⇒ '(2 4 2 6)



;;
;;3.B
;;

(define (todas-hojas-cumplen? pred arbol)
  (if (hoja-arbol? arbol)
      (if (pred (dato-arbol arbol))
          #t
          #f)
      (todas-hojas-cumplen-bosque? pred (hijos-arbol arbol))))

(define (todas-hojas-cumplen-bosque? pred bosque)
  (if (null? bosque)
      #t
      (and (todas-hojas-cumplen? pred (car bosque))
           (todas-hojas-cumplen-bosque? pred (cdr bosque)))))



;(todas-hojas-cumplen? even? arbol1) ; ⇒ #f
;(todas-hojas-cumplen? even? arbol2) ; ⇒ #t

(define (todas-hojas-cumplen-fos? pred arbol)
  (for-all? (lambda (elem) (if (hoja-arbol? elem)
                               (pred (dato-arbol elem))                                  
                               (todas-hojas-cumplen-fos? pred elem))) (hijos-arbol arbol)))

;(todas-hojas-cumplen-fos? even? arbol2) ; ⇒ #t


;;;::::::::::::
;;;EJERCICIO 4:
;;;::::::::::::

;;
;;4.A
;;

(define (suma-raices-hijos arbol)
  (foldr + 0 (map dato-arbol (hijos-arbol arbol))))

(define (raices-mayores-bosque? bosque)
  (if (null? bosque)
      #t
      (and (raices-mayores-arbol? (car bosque))
           (raices-mayores-bosque? (cdr bosque)))))

(define (raices-mayores-arbol? arbol)
  (and (> (dato-arbol arbol) (suma-raices-hijos arbol))
       (raices-mayores-bosque? (hijos-arbol arbol))))


(define (raices-mayores-arbol-fos? arbol)
  (and (> (dato-arbol arbol) (suma-raices-hijos arbol)) (for-all? raices-mayores-arbol-fos? (hijos-arbol arbol)))) 

(define arbol3 '(20 (2) (8 (4) (2)) (9 (5))))

;(raices-mayores-arbol-fos? arbol3) ; ⇒ #t
;(raices-mayores-arbol-fos? '(20 (2) (8 (4) (5)) (9 (5)))) ; ⇒ #f


(define (comprueba-raices-bosque bosque)
  (if (null? bosque)
      '()
      (cons (comprueba-raices-arbol (car bosque))
            (comprueba-raices-bosque (cdr bosque)))))
  


(define (comprueba-raices-arbol arbol)
  (if (> (dato-arbol arbol) (suma-raices-hijos arbol))
      (nuevo-arbol 1 (comprueba-raices-bosque (hijos-arbol arbol)))
      (nuevo-arbol 0 (comprueba-raices-bosque (hijos-arbol arbol)))))


(comprueba-raices-arbol arbol3) ; ⇒ (1 (1) (1 (1) (1)) (1 (1)))
(comprueba-raices-arbol '(20 (2) (8 (4) (5)) (9 (5)))) 
