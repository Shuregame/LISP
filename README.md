# LISP

## Задача 9
	Определите функцию, разделяющую исходный список на два подсписка. В первый из них должны попасть элементы с нечетными номерами, во второй — элементы с четными номерами.

```Lisp

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

(print(list-sublist`(1 2 3)))
((1 3) (2)) 

(print(list-sublist`(1 2 3 4 5 6 7 8)))
((1 3 5 7) (2 4 6 8)) 
```
