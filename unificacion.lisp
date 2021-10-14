; Diego Sánchez Martín
; Fernando Olivares Naranjo
; © 2021: All Rights Reserved

(defun esAtomo(dato)
  (if (eq (listp dato) 'T) ;Primero ver si es lista o átomo
    (if (eq (first dato) '?) ;Si lista, mirar si es constante o variable
      (princ "Constante introducida")
      (princ "Variable introducida")
    )
    (princ "Variable introducida") ;Sino entonces es variable seguro
  )
)

(defun unificacion(e1 e2)
  (if (eq (atom e1) 'T)
    (bucle e1 e2) ; Si e1 es átomo vamos a continuar
    (if (eq (atom e2) 'T) ; Else en caso de que E1 no sea átomo
      (intercambiar e1 e2) ; Si E2 es atomo y E1 no, intercambiamos
      (continuar e1 e2)
    )
  )

  (print "")
)

(defun intercambiar(e1 e2) ; También se puede hacer (bucle e2 e1) directamente
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

(defun prueba (e1 e2)
  (setq f1 (first e1))
  (setq t1 (rest e1))
  (setq f2 (first e2))
  (setq t2 (rest e2))
  (write f1)
  (write t1)
  (write f2)
  (write t2)
  (setq z1 (prueba (f1 f2)))

  (unless (eq e1 e2)
    (if (esVariable e1)
      (bucle )
      (if (esVariable e2)
        (devolver )
        (devolverFallo)
      )
    )
  )
)

(defun prueba2 (dato)
  (setq dato '2)
  (princ "El nuevo valor de E2 en prueba2 es: ")
  (write dato)
)
