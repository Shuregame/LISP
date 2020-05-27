;;; ФУНКЦИИ ВЫСШИХ ПОРЯДКОВ

;;; 14 Задача
;;; Определите функцию, которая возвращает в 
;;; качестве значения форму своего определения (DEFUN).

(defun progg (&rest rest)
'(defun progg (&rest rest))
)

;;; Test 1
(write-line "Задача 14 Test 1")
(princ " >> (progg 'a 'b)")
(print (progg 'a 'b))
(write-line "")
(write-line "")



;;; 4 Задача
;;; Определите функциональный предикат (КАЖДЫй пред список), 
;;; который истинен в том и только в том случае, когда, 
;;; являющейся функциональным аргументом предикат пред истинен 
;;; для всех элементов списка список.

(defun prov(lst)
(null
(mapcan
(lambda (x)
(if (funcall #'numberp x) nil (list t))
)lst
)
)
)

;;; Test 1
(write-line "Задача 4 Test 1")
(princ " >> (1 2 a)")
(print (prov '(1 2 a)))
(write-line "")
(write-line "")

;;; Test 2
(write-line "Задача 4 Test 2")
(princ " >> (1 2 2)")
(print (prov '(1 2 2)))
(write-line "")
(write-line "")

;;; 6 Задача
;;; Определите фильтр (УДАЛйЬ-ЕСЛИ пред список), удаляющий из списка список
;;; все элементы, которые обладают свойством, наличие которого проверяет предикат пред.

(defun del (lst)
(mapcan (lambda(x)
(if (funcall 'numberp x) nil (list x))) lst)
)

;;; Test 1
(write-line "Задача 6 Test 1")
(princ " >> ( 1 3 4 a)")
(print(del '( 1 3 4 a)))
(write-line "")
(write-line "")

;;; Test 2
(write-line "Задача 6 Test 2")
(princ " >> ( 1 3 4 1 4 10 b)")
(print(del '( 1 3 4 1 4 10 b)))
(write-line "")
(write-line "")


;;; 8 Задача
;;; Напишите генератор натуральных чисел: 0, 1, 2, 3, 4, 5, ...

(defun generator ()
(let
((numb -1))
(lambda () (setq numb (+ numb 1)))
)
)
(setq c1 (generator))

(defun self (&rest rest)
(append '(self) rest)
)

;;; Test 1
(write-line "Задача 8 Test 1")
(princ " >> (funcall c1)")
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(write-line "")
(write-line "")


;;; 12 Определите функцию, которая возвращает в качестве значения свой вызов.

(defun self (&rest rest)
	(append '(self) rest)
)
       
;;; Test 1
(write-line "Задача 12 Test 1")
(princ ">> (self 1 2)")
(print (self 1 2)) 

;;; 2 Определите функицонал (MAPLIST fn список) для одного списочного аргумента ; ОШИБКА!!!

(defun maplist1 (fn lst)
          (cond 
             ((null fn) nil)
             ((null lst) nil)
            
             (t(cons (funcall fn lst)   (maplist1 fn (cdr lst))))    ;->((3 2 1) (3 2) (3)) 
            ;(t(append (funcall fn lst) (maplist fn (cdr lst))))    ;->(3 2 1 3 2 3)
          )
)


;;; Test 1
(write-line "Задача 2 Test 1")
(princ ">> (1 2 3)")
(print (maplist1 'REVERSE '(1 2 3)))
(write-line "")
(write-line "")

;;; Test 2
(write-line "Задача 2 Test 2")
(princ ">> (1 2 3 4 5)")
(print (maplist1 'REVERSE '((1 2 3 4 5)))
(write-line "")
(write-line "")

;;; 10 Напишите генератор, порождающий последовательность (A), (B A), (A B A), (B A B A)

(defun generator ()
     (let ( (x 'A) (y 'B) (z ()))
         (lambda () (if (equal x (car z))
                    (setq z (cons y z))
                    (setq z (cons x z))   
                       ))
     )
 )
(setq c1 (generator))

;;; Test 1
(write-line "Задача 10 Test 1")

(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))

(write-line "")
(write-line "")
