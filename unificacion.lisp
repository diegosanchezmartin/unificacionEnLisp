; Diego Sánchez Martín
; Fernando Olivares Naranjo
; © 2021: All Rights Reserved

(defun unificacion(e1 e2)
  (if (eq (or (null e1) (null e2)) 'NIL)
    (if (eq (atomoUnificacion e1) 'T) ;Miramos si e1 es átomo
      (unless (equalp e1 e2) ;Importante aquí saber que si son iguales abajo devuelve un NIL donde z1 y z2
        (if (eq (esVariable e1) 'T)
          (if (aparece e1 e2)
            (princ "error")
            (list e2 '/ e1)
          )
          (if (esVariable e2)
            (list e1 '/ e2)
            (princ "FALLO")
          )
        )
      )
      (if (eq (atomoUnificacion e2) 'T) ;Si e1 no es átomo, miramos si lo es e2
        (unless (equalp e2 e1)
          (if (eq (esVariable e2) 'T)
            (if (aparece e2 e1)
              (princ "error")
              (list e1 '/ e2)
            )
            (if (esVariable e1)
              (list e2 '/ e1)
              (princ "FALLO")
            )
          )
        )
        (continuacion e1 e2)
      ) ;Si ninguno de los dos es, el bucle saldrá
    )
    NIL
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
  (write z1)
  (format t "~%")
  (if (eq z1 'T) ;Hay que poner arriba que si da error devuelva T porque el unless devuelve NIL siempre
    (princ "FALLO Z1")
  )
  (setq g1 (aplicar z1 t1))
  (princ "G1: ")
  (write g1)
  (format t "~%")
  (setq g2 (aplicar z1 t2))
  (princ "G2: ")
  (write g2)
  (format t "~%")

  (setq z2 (unificacion g1 g2))
  (write z2)
  (format t "~%")
  (if (eq z2 'T) ;Aquí lo mismo que arriba
    (princ "FALLO Z2")
  )
  ;Ponemos unless porque con un if, habría que meter un return 0 por ejemplo dentro del if para salir del todo y en lisp es imposible
  ;(princ "Aqui debemos parar ")
  ;(format t "~%")

  (componer z1 z2)
)

(defun aplicar (expresion dato)
  (unless (eq expresion 'NIL) ;Si la expresion es NIL es que arriba se ha devuelto: NADA

    ;(setq datoExpresion (last expresion))
    ;(write datoExpresion)
    ;(format t "~%")
    ;(write dato)
    ;(format t "~%")
    ;(setq prueba1 (atomoUnificacion dato))
    ;(write prueba1)
    ;(format t "~%")

    (if (eq (atomoUnificacion dato) 'T)
      (if (equalp dato datoExpresion) ;equalp porque tienen que ser iguales conceptualmente
        (first expresion)
        dato
      )
    )
  )
  dato
)

(defun componer (dato1 dato2)
  (list dato1 '- dato2)
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

(defun esVariable(dato)
  (if (eq (listp dato) 'T) ;Primero ver si es lista o átomo
    (if (eq (first dato) '?) ;Si lista, mirar si es constante o variable
      T
      NIL
    )
    NIL
  )
)
