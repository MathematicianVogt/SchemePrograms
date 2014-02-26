;#1 a merge function that takes two listes already ordered and combines them in such a way that the result is ordered
;carList function, takes an element and appends it to the end of a given list, element, list -> list
(define (carList carL FL) (append FL (list carL)))
;mergeFunction takes two lists, and a final list, recursively merges the lists such that the final product is sorted in asencding order list1 list2 list3 -> finalList
(define (mergeFunction L1 L2 LF) (if(and (null? L1) (null? L2) ) LF (if(null? L1) (append LF L2) (if(null? L2) (append LF L1) (if(<= (car L1) (car L2)) (mergeFunction (cdr L1) L2 (carList (car L1) LF)) (mergeFunction L1 (cdr L2) (carList (car L2) LF))  ))))) 
;merge takes two lists, and a final list, recursively merges the lists such that the final product is sorted in asencding order list1 list2-> finalList
(define (merge ordered1 ordered2)(mergeFunction ordered1 ordered2 '()))
;(merge '(5 5 5 7 9 10) '(1 2 4 5 6 6 8 9 10 11 12 13))
;#2 a merge-sort function that takes an unsorted list and uses the merge sort algorithm with also the merge function above to sort a list
;oddElements list-> list takes a list and returns the odd elements of the list
(define (oddElements L) (if(null? L) '() (if(null? (cdr L)) (list (car L)) (cons (car L) (oddElements (cdr (cdr L))))))) 
;evenElements-list-> list gives the even elements of a list
(define (evenElements L ) (if(null? L) '() (if (null? (cdr L)) '() (cons (car (cdr L ) ) (evenElements (cdr (cdr L)))))))
;merge-sort list->list takes a list that is not ordered and orders it in acending order by mergesort algorith
;takes advatage of odd and even element functions to divide lists and sort them on the fly
(define (merge-sort L ) (if(null? L ) L (if(null?(cdr L )) L ( merge (merge-sort (oddElements L )) (merge-sort (evenElements L))))))
(merge-sort '(5 632 6 212 4 5 3 5 7 1 7 1 1 1))
;#3 furthering the deriv function in class to also accept power functions arthExp var -> list
; variable?: Any -> Bool
(define variable? symbol?)

; (eq? (variable? 'x) #t)
; (eq? (variable? 3) #f)

; variable=?: VarExp * VarExp -> Bool
(define variable=? eq?)

; (variable=? 'x 'x)
; (not (variable=? 'x 'y))

;; a sum is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol +

; sum?: Any -> Bool
(define (sum? a) (and (pair? a) (eq? (car a) '+)))

; (eq? (sum? '(+ 2 3)) #t)
; (eq? (sum? '3) #f)

; make-sum: ArithExp * ArithExp -> SumExp
(define (make-sum e1 e2) (list '+ e1 e2))

; (equal? (make-sum 2 3) '(+ 2 3))

;; a product is represented as a list with three elements: tag, e1, e2.
;; the tag is the symbol *

; product?: Any -> Bool
(define (product? a) (and (pair? a) (eq? (car a) '*)))

; (eq? (product? '(* 2 3)) #t)
; (eq? (product? '3) #f)

; make-sum: ArithExp * ArithExp -> ProductExp
(define (make-product e1 e2) (list '* e1 e2))

; (equal? (make-product 2 3) '(* 2 3))

;; sums and products will use the same selectors

; arg1: SumExp or ProdExp -> ArithExp
(define (arg1 e) (car (cdr e)))

; (= (arg1 (make-sum 2 3)) 2)
; (= (arg1 (make-product 2 3)) 2)

; arg2: SumExp or ProdExp -> ArithExp
(define (arg2 e) (car (cdr (cdr e))))

; (= (arg2 (make-sum 2 3)) 3)
; (= (arg2 (make-product 2 3)) 3)




;make-expr: arithExp * natutalNumber -> arithExp
(define (make-expt e n) (cons '^ (append (list e) (list n))))
(make-expt '(* x 5) 2)
;expt? arthimExp-> Bool a function to see if a arthimac expression is in a correct form
(define (expt? e) (and (pair? e) (eq? (car e) '^)))
;make-expt arithExp->arithExp takes a power function and differentiates it. 
(define (make-exptDev L v )(if(or (number? (car L)) (number? (car(cdr L)))) (list 0) (if(= (arg2 L) 0) (list 0) (if(= (arg2 L) 1) (list (deriv (arg1 L) v)) (list '*  (arg2 L) (list '^ (arg1 L) (- (arg2 L) 1)))))))
;;; derivative code

;deriv: ArithExp * VarExp -> ArithExp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (variable=? exp var) 1 0))
        ((expt? exp) (list '* (make-exptDev exp var) (deriv (arg1 exp) var)))
        ((sum? exp) 
         (make-sum (deriv (arg1 exp) var)
                   (deriv (arg2 exp) var)))
        ((product? exp)
         (make-sum (make-product (arg1 exp) (deriv (arg2 exp) var))
                   (make-product (arg2 exp) (deriv (arg1 exp) var))))
        (else (error 'deriv "Unexpected Input, not an ArithExp"))))


(deriv '(* (^ x 100) x) 'y)
;#4

