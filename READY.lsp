;9 
;Определите функцию, разделяющую исходный список на два подсписка. 
;В первый из них должны попасть элементы с нечетными номерами, 
;во второй — элементы с четными номерами.
;(1 2 3) -> ((1 3) (2))


(defun list-sublist(lst)
     (cond
          ((null (cdr lst)) (list lst))
	       (t 
		 ((lambda (lst1)
		    (list
			(cons (car lst) (car lst1))
			(cons (cadr lst) (cadr lst1))))
		(list-sublist (cddr lst)))
	)
)
)



(write-line "Задача 9")
;;; Test 1
(princ " >> (1 2 3) ")
(print(list-sublist`(1 2 3)))
(write-line "")
(write-line "")

(write-line "Задача 9")
;;; Test 2
(princ " >> (1 2 3 4 5 6 7 8) ")
(print(list-sublist`(1 2 3 4 5 6 7 8)))
(write-line "")
(write-line "")



; 11
; Определите функцию, осуществляющую разделение
;исходного списка на два подсписка. 
; В первый из них должно попасть указанное количество элементов
; с начала списка, во второй — оставшиеся элементы.
; (9 1 8 2) 2 -> ((9 1) (8 2))


(defun split (lst k)
    (cond
        ((null lst) nil)
            (t
              ((lambda (first res)
                  ((lambda (x y)
                       (cond
                           ((> k 0)
                                (cons (cons first x) (cdr y)))
                            (t  
                                (list x (cons first (cadr y))))
                        )
                     )(car (split res (- k 1))) (split res (- k 1)))
                )(car lst)(cdr lst))
         )
    )
)

(write-line "Задача 11")
;;; Test 1
(write-line "Test 1")
(princ " >> (1 2 3) 2)")
(print (split '(1 2 3) 2)
)
(write-line "")
;;; Test 2
(write-line "Test 2")
(princ " >> (1 2 3) 3)")
(print (split '(1 2 3) 3))
(write-line "")

(write-line "")

;3
;Определите функцию, заменяющую в исходном списке все вхождения заданного значения другими

(print "-----TASK 3-----")

(defun rep (lst pattern replaceWith)
	(
    	(lambda(head tail)
    	
            (cond 
        		(
        			(null lst)
        			nil
        		)
        	    (
        	        (eq head pattern) 
        		    (cons replaceWith (rep tail pattern replaceWith))
        		)
    		    (t
    		        (cons head (rep tail pattern replaceWith))
    		    )
    		)
    	)
    	(car lst)(cdr lst)
	)
)

;Test cases
(print (rep '(1 2 3 1 1) 1 'a))
;(A 2 3 A A) 
(print (rep '((1 2 3) 4 5 6 1) 1 44))
;((1 2 3) 4 5 6 44) 


;20
;Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом списка. Пример:
;> (ПЕРВЫЙ-АТОМ ’(((a b)) c d))
;A
(print "-----TASK 20-----")

(defun ПЕРВЫЙ-АТОМ(lst)
	(cond
        (
			(atom lst) 
			lst
		)
		(t 
			(ПЕРВЫЙ-АТОМ (car lst))
		)
    )
)

;Test cases

(print (ПЕРВЫЙ-АТОМ '(1)))
; 1
(print (ПЕРВЫЙ-АТОМ '((+ 1 2) 5)))
; +


;№46
;Предположим, что отец и мать некоторого лица, хранятся как значения соответствующих свойств у символа, обозначающего это лицо. 
;Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, и предикат (СЕСТРЫ-БРАТЬЯ x1 x2),
;который истинен в случае, если x1 и x2 — сестры или братья, родные или с одним общим родителем.
( setf ( get 'C 'mother) 'A)
( setf ( get 'C 'father) 'B)

( setf ( get 'H 'mother) 'E)
( setf ( get 'H 'father) 'R)

( setf ( get 'K 'mother) 'M)
( setf ( get 'K 'father) 'N)

( setf ( get 'Q 'mother) 'M)
( setf ( get 'Q 'father) 'N)

(defun get-mother(x)
(get x 'mother)
)

(defun get-father(x)
(get x 'father)
)

(defun parents(x)
(list (get-mother x) (get-father x))
)

(defun sisters-brothers(x1 x2)
    (cond
        ((STRING= (get x1 'father) (get x2 'father))t)
        ((STRING= (get x1 'mother) (get x2 'mother))t)
        (t nil)
    )
)

; A  B      E  R      M  N
;  \/        \/        \/
;  С         H        K  Q


(print(parents 'C))
(print(parents 'H))
(print(parents 'Q))
(print(parents 'K))

(print(sisters-brothers 'H 'K))
(print(sisters-brothers 'K 'Q))

; 45 Задача
; Определить расстояние между двумя точками. У символа (названия города) определены свойства x и y,
; необходимо вычислить расстояние между двумя городами.
; struct city {
;   float x, y;
; }
; length_city (city, city) -> float

(defun length_city (city-a city-b)
    (sqrt (+
        (expt
            (- (get city-a 'x) (get city-b 'x))
        2)
        (expt
            (- (get city-a 'y) (get city-b 'y))
        2)
    ))
)
(defun distance-city (name x y)
        (setf (get name 'x) x)
        (setf (get name 'y) y)
)


; Test 1
(write-line "Задача 45 Test 1")
(princ ">> {0, 5} {7, 0}")
(distance-city 'city1 0 5)
(distance-city 'city2 7 0)
(print (length_city 'city1 'city2))
(write-line "")
(write-line "")

; Test 2
(write-line "Задача 45 Test 2")
(princ ">> {32, 19} {58, 26}")
(distance-city 'city1 32 19)
(distance-city 'city2 58 26)
(print (length_city 'city1 'city2))
(write-line "")
(write-line "")

; 28. Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре).

(defun atomCnt (l)
((lambda (last)
  (cond ((null l) 0)
    ((atom (car l)) (+ 1 (atomCnt last)))
    (t (atomCnt last))))
    (cdr l)); последний = (cdr l)
    )
	
(print(atomCnt '(2 91 3 5)))
(print(atomCnt '(2 (4) 1 1)))
