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
    (bucle e1 e2) ; Si e1 es átomo vamos a continuar
    (if (eq (atom e2) 'T) ;else
      (intercambiar e1 e2)
      (continuar e1 e2)
  )

  (print "")
)

(defun intercambiar(e1 e2)
  (setq aux e1)
  (setq e1 e2)
  (setq e2 aux)
  (bucle e1 e2) ; Si e2 es átomo vamos a continuar
)

(defun bucle(e1 e2) ; Esto sería el trozo entre el BEGIN y el END
  (if (equalp e1 e2))
    
)

(defun continuar(e1 e2) ; Esto sería el trozo a partir de la línea 12
  (setq f1 (first e1))
  (setq t1 (rest e1))
  (setq f2 (first e2))
  (setq t2 (rest e2))
  (setq z1 (unificacion (f1 f2)))
)
