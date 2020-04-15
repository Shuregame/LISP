;9 Задача

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


;;; Test 1
(write-line "Задача 9 Test 1")
(princ " >> (1 2 3) ")
(print(list-sublist`(1 2 3)))
(write-line "")
(write-line "")


;;; Test 2
(write-line "Задача 9 Test 1")
(princ " >> (1 2 3 4 5 6 7 8) ")
(print(list-sublist`(1 2 3 4 5 6 7 8)))
(write-line "")
(write-line "")
