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
