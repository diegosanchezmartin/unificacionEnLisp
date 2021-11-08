; Diego Sánchez Martín
; Fernando Olivares Naranjo
; © 2021: All Rights Reserved

(load "unificacion.lisp")
(aparece 'x '(x y))
(aparece 'x '(u y))
(esvariable 'x)
(esvariable '(? x))
(esvariable '(y x))
(esvariable '(y (? x)))
(atomoUnificacion 'x)
(atomoUnificacion '(? y))
(atomoUnificacion '(y y))
(atomoUnificacion '(y (? y)))
(sustituir '(G / (? x)) '(G) '0)
(sustituir '(G / (? x)) '(? x) '0)
(continuacion '((? x) A) '(Y (? z)))
(unificacion '((? x) A) '(Y (? z)))
(unificar '(p (f x y) (? z)) '((? x) (? r) b))
(format t "~%")
(unificar '((? x) (Y Z) P (? q)) '(L (? b) (? u) (R S T)))
(format t "~%")
(unificar '((? y) (G (? y))) '(Z (G Z)))
(format t "~%")
(unificar 'A 'Y)
(format t "~%")
(unificar '(A (? x)) '(Y (? z)))
(format t "~%")

; Las funciones continuacion y unificacion dan NIL debido a que son parte
; de la función unificar, la cual inicializa el valor de listaFinal y es
; por ello que la ejecución de estas funciones está condicionada si no
; llamamos previamente a esta función
