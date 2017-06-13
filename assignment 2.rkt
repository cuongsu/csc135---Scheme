;Cuong Su
;CSC 135
;Assignment 2
;*****************I used Pretty Big language for this assignment.**********************

;Problem A
;"buildLR" - takes two integers, and returns a 4-digit integer constructed of the leftmost 2 digits of the
;first input, and the rightmost 2 digits of the second input. For example, (buildLR 78432 139) would
;return 7839. Negative signs on either input number should be ignored - that is, (buildLR 78432 -139)
;would also return 7839. If either number has less than two digits, your function should return -1.

(define (absoluteValue x)
  (if (< x 0) (* x -1) x))

;lessThan2 checks whether the int x has more than 2 digits. If it doesn't, return -1. Otherwise return x.
(define (lessThan2 x)
  (if (and (> x -10) (< x 10)) -1 x))

;First2 returns the first two digits of a number by recursively dividing by 10, whenever it has more than two digits. 
(define (first2 x)
  (if (< (absoluteValue x) 99) x
      (floor (first2 (/ (absoluteValue x) 10)))))

;if one of the two numbers have less than two digits, then instantly return -1. Otherwise return the first two
;digits of the first number, multipled by 100, plus the last two digits of the second number.
(define (buildLR x y)
  (if (or (= (lessThan2 (absoluteValue x)) -1) (= (lessThan2 (absoluteValue y)) -1)) -1
    (+ (* (absoluteValue (first2 x)) 100) (modulo (absoluteValue y) 100))))



;Problem B
;"listMins" - takes two equal-length lists of numbers, and returns a single list consisting of the smaller of
;the two lists, position by position. For example, (listMins '(2 8 7) '(5 4 4)) would return the list (2 4 4),
;because 2<5, 4<8, and 4<7.

(define (listMins L1 L2)
  (if (or (null? L1)(null? L2)) ()
      (if (< (car L1) (car L2)) (cons (car L1) (listMins (cdr L1) (cdr L2)))
          (cons (car L2) (listMins (cdr L1) (cdr L2))))))  



;Problem C
;"unwind" - takes a list of atoms and returns the unwinded version of the list. "Unwinding" a list means
;extracting the two centermost elements and putting them at the front of the list, then repeating the
;process until the entire list is "unwound". For example, (unwind '(7 8 2 9 5 6)) should return the list (2
;9 8 5 7 6). If the list has an odd number of elements, the centermost value should be extracted first,
;then the algorithm proceeds normally. For example, (unwind '(7 8 2 3 9 5 6)) would return (3 2 9 8 5 7
;6).

;mid finds the middle index of the list and returns that index.
(define (mid L)
  (floor (/ (- (length L) 1) 2)))

;remove-ref returns a list after removing a value n, and putting n at the front of the list.
(define (remove-ref L n)
  (if (= n 0) (cdr L)
      (cons (car L) (remove-ref (cdr L) (- n 1)))))

;if the list is null, return an empty list. Otherwise, recrusively put the value of the middle index of a list
;at the front until you get to the last number in the original list.
(define (unwind L)
  (if (null? L) '()
      (cons (list-ref L (mid L)) (unwind (remove-ref L (mid L))))))
      


;Probem D
;"functionWinner" - takes two Boolean functions F and G, and a list L. It returns the number 1 if
;function F is the "winner", and the number 2 if function G is the "winner", and the number 0 if the two
;functions are tied. The winning function is the one which has the most "true" answers for each value
;in the list. For example, (functionWinner isNeg isEven '(7 -3 2 1 -5)) returns 1, because the "isNeg"
;function is the winner - two of the numbers in L are negative, but only one of the numbers is even. If
;both of the functions are true for the same number of cases, then the two functions are "tied", and the
;value 0 is returned.

;functionWinnerCheck checks whether the results of Boolean function F and G return true/false and returns either a negative
;number, positive number, or zero. If the list is null, return 0. If F returns true and G returns false, add 1. If G returns
;true and F returns false, subtract 1. Otherwise just recursive call checker with the rest of the list.
(define (functionWinnerCheck F G)
  (define (checker L)
    (if (null? L) 0
      (if (and (eqv? (F (car L)) #t) (eqv? (G (car L)) #f)) (+ 1 (checker (cdr L)))
          (if (and (eqv? (G (car L)) #t) (eqv? (F (car L)) #f)) (- (checker (cdr L)) 1)
              (checker (cdr L))))))
  checker
)

;functionWinner takes the results of the check and returns 0 if the result of the check was 0, 1 if the check returned a positive
;number, or 2 if the check returned a negative number.
(define (functionWinner F G L)
  (if (= 0 ((functionWinnerCheck F G) L)) 0
      (if (positive? ((functionWinnerCheck F G) L)) 1
         (if (negative? ((functionWinnerCheck F G) L)) 2)))) 

;checks whether x is negative.
(define (isNegative x) (< x 0))

;checks whether x is even.
(define (isEven x)(= 0 (modulo x 2)))



;Problem E
;"getNestedCount" - takes a list of integers, possibly including nested lists, and returns the total number
;of integers in the entire nested list. For example, (getNestedCount '(2 3 (4 (7 6) 5))) should return 6.
;You will find it useful to use the "list?" function, which works as follows: (list? L) returns true if L is a
;list.

;getNestedCount returns 0 if the list is null. Otherwise, it checks if the next thing in the list is a list.
;If it is, then recurse until you hit the end of the list while adding 1 to the total. If it isn't,
;then just add 1 and recurse.
(define (getNestedCount L)
  (if (null? L) 0
      (if (list? (car L)) (+ (getNestedCount (car L)) (getNestedCount (cdr L)))
          (+ 1 (getNestedCount (cdr L))))))



;Problem F
;A function "makeCutter", that takes as input an integer N. It then builds and returns a "cutter"
;function based on N. The "cutter" function that is produced (by your function makeCutter) would have
;the property that it takes as input a list, and returns the first N items from that list. For example, if
;makeCutter was called as follows:
;(makeCutter 3)
;a function would be produced that takes as input a list and returns the first 3 elements from that list.
;For example, if the original call had been made as follows:
;(define C (makeCutter 3))
;then the produced function C would behave as follows:
;(C '(4 8 2 9 -1 13)) *** would return (4 8 2)
;(C '(-2 3 -4 8 9 1 7)) *** would return (-2 3 -4)
;Your task is just to write makeCutter.
;Of course, makeCutter should work for ANY input integer, not just 3.

;decrement returns an empty list if the list is null. Otherwise if N is greater than 0, recurse with N-1 and the
;cdr of the list as parameters. If N is less than 0, return an empty list.
(define (decrement N L)
    (if (null? L) '()
      (if (> N 0) (cons (car L) (decrement (- N 1) (cdr L)))
          '())))

;makeCutter basically calls lambda, which calls decrement.
(define (makeCutter N) (lambda (L) (decrement N L)))
 

