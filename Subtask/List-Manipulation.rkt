#lang racket

;Section 3 Part 2 (Scheme) Sum A List
;We use simple recursion to go through every element in the list.
;The procedure adds the current head of the list, and then calls
;the procedure again with the tail of the list being the new parameter.

;Base Case: Lst is empty. ;This will happen when every element in the list has been visited.
;Recursive Case: Head of List + Recursion Result of the Tail of the List.
(define (sum Lst)
  (if (empty? Lst)
      0 ;Returns 0 when the list is empty 
      (+ (car Lst) (sum (cdr Lst))) ;Performs the arithmetic operation with recursion
      )
  )

;Test Cases For Section 3 Part 2 (Scheme) Sum A List
;(sum '(1 2 3))
;6

;(sum '())
;0

;(sum '(1 5 9))
;15


;Section 3 Part 4 (Scheme) Check A List Is In Descending Order
;We use recursion again as it follows the same format as (sum Lst) where we have to go
;through each element of the list
;When the remainder of the Lst is empty, we will return true because it can only get to that point
;when every element is in descending order
;In a descending list, the head of the list has to be larger than or equal to the first element of the tail of the list.

;Base Case: (cdr Lst) is empty (The tail of the list is empty, we check if the tail is empty as its more efficient to check if the Lst is empty
;as that would add more depth to the recursion).
;Recursive Case: When Head is >= Head of Tail of list, (desc? (cdr Lst)) meaning use recursion with the Tail of the list.
(define (desc? Lst)
  (if (empty? (cdr Lst))
      #t
      (if (or (> (car Lst) (car (cdr Lst))) (= (car Lst) (car (cdr Lst)))) ;If Head is larger than or equal to the First element of the Tail of the list
              (desc? (cdr Lst)) ;Recursive case when if statement is true
              #f ;Returns false when if statement fails
              )
      )
  )

;Test Cases For Section 3 Part 4 (Scheme) Check A List Is In Descending Order
;(desc? '(89 78 65 45))
;#t

;(desc? '(89 45 65 79))
;#f