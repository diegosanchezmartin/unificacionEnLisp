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

(defun intercambiar(e1 e2) ; También se puede hacer (bucle e2 e1) directamente
  (setq aux e1)
  (setq e1 e2)
  (setq e2 aux)
  (bucle e1 e2) ; Si e2 es átomo vamos a continuar
)

(defun continuar(e1 e2) ; Esto sería el trozo a partir de la línea 12
  (setq f1 (first e1))
  (setq t1 (rest e1))
  (setq f2 (first e2))
  (setq t2 (rest e2))
  (setq z1 (unificacion (f1 f2)))
)

(defun unificacion(e1 e2)

  (if (eq (atomoUnificacion e1) 'T) ;Miramos si e1 es átomo
    (unless (equalp e1 e2)
      (if (eq (esVariable e1) 'T)
        (if (aparece e1 e2)
          (princ "error")
          (imprimir e2 e1)
        )
        (if (esVariable e2)
          (imprimir e1 e2)
          (princ "FALLO")
        )
      )
    )
    (if (eq (atomoUnificacion e2) 'T) ;Si e1 no es átomo, miramos si lo es e2
      (unless (equalp e2 e1)
        (if (eq (esVariable e2) 'T)
          (if (aparece e2 e1)
            (princ "error")
            (imprimir e1 e2)
          )
          (if (esVariable e1)
            (imprimir e2 e1)
            (princ "FALLO")
          )
        )
      )
      (continuacion e1 e2)
    ) ;Si ninguno de los dos es, el bucle saldrá
  )
)

(defun continuacion (e1 e2)
  (setq f1 (first e1))
  (setq t1 (rest e1))
  (setq f2 (first e2))
  (setq t2 (rest e2))
  (write f1)
  (format t " ")
  (write t1)
  (format t " - ")
  (write f2)
  (format t " ")
  (write t2)
  (format t " ")
  (format t "~%")
  (setq z1 (unificacion f1 f2))
  (setq z2 (unificacion t1 t2))
)

(defun atomoUnificacion (dato)
  (if (eq (listp dato) 'T) ;Primero ver si es lista o átomo
    (if (eq (first dato) '?) ;Si lista, mirar si es constante o variable
      T
      NIL
    )
    T
  )
)

(defun aparece (dato lista)
  (unless (atom lista) ;Esto es básicamente porque si ponemos (unificar 1 2) nos va a saltar aquí y como el 2 es átomo la función member va a devolver error
    (member dato lista)
  )
)

(defun imprimir (e1 e2)
  ;(write e2)
  ;(princ "/")
  ;(write e1)
  (list e1 '/ e2)
)

(defun esVariable(dato)
  (if (eq (listp dato) 'T) ;Primero ver si es lista o átomo
    (if (eq (first dato) '?) ;Si lista, mirar si es constante o variable
      T
      NIL
    )
    NIL
  )
)
