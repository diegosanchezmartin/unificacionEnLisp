; Diego Sánchez Martín
; Fernando Olivares Naranjo
; © 2021: All Rights Reserved

(defun esAtomo(dato)
  (if (eq (first dato) '?)
    (princ "Constante introducida")
    (princ "Variable introducida"))
)

(defun unificacion(e1 e2)
  (if (eq (atom e1) 'T)
    (print "e1 es atomo") ;if
    (if (eq (atom e2) 'T) ;else
      (intercambiar e1 e2)
      continuar(e1 e2)
  )

  (print "")
)

(defun intercambiar(e1 e2)
  (setq aux e1)
  (setq e1 e2)
  (setq e2 aux)
)
