; Diego Sánchez Martín
; Fernando Olivares Naranjo
; © 2021: All Rights Reserved

(defun unificar(e1 e2)
  (setq listaFinal NIL)
  (unificacion e1 e2)
  (write listaFinal)
)

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
  (if (eq listaFinal 'NIL) ;En caso de que listaFinal
    (unless (eq z1 'NIL) ;
      (setq listaFinal z1) ;
    )
    (unless (eq z1 'NIL)
      (setq listaFinal (list listaFinal z1))
    )
  )
  (if (eq z1 'T) ;Hay que poner arriba que si da error devuelva T porque el unless devuelve NIL siempre
    (princ "FALLO Z1")
  )
  (if (eq z1 'NIL)
    (setq g1 t1)
    (setq g1 (sustituir z1 t1 0))
  )
  (princ "G1: ")
  (write g1)
  (format t "~%")

  (if (eq z1 'NIL)
    (setq g2 t2)
    (setq g2 (sustituir z1 t2 0))
  )
  (princ "G2: ")
  (write g2)
  (format t "~%")

  (setq z2 (unificacion g1 g2))
  (write z2)
  (format t "~%")
  (if (eq listaFinal 'NIL) ;En caso de que listaFinal
    (unless (eq z2 'NIL) ;
      (setq listaFinal z2) ;
    )
    (unless (eq z2 'NIL)
      (setq listaFinal (list listaFinal z2))
    )
  )
  (if (eq z2 'T) ;Aquí lo mismo que arriba
    (princ "FALLO Z2")
  )
  ;Ponemos unless porque con un if, habría que meter un return 0 por ejemplo dentro del if para salir del todo y en lisp es imposible
  ;(princ "Aqui debemos parar ")
  ;(format t "~%")

  (write listaFinal)
  (format t "~%")
  ;(write z1)
  ;(format t "~%")
  ;(write z2)
  ;(format t "~%")
  ;(componer z1 z2)
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

(defun sustituir (expresion valorAcomprobar bandera)
  (unless (null expresion)
    (setq valor (first (last expresion)))
    (write valor)
    (format t "~%")
    (if (eq (esVariable valorAcomprobar) 'T)
      (write valorAcomprobar)
      (write (first valorAcomprobar))
    )
    (format t "~%")
    (if (equalp valor (first valorAcomprobar))
      (if (eq (atom (first expresion)) 'T)
        (if (eq (length valorAcomprobar) 1)
          (if (= bandera 0)
            (list (first expresion))
            (first expresion)
          )
          (list (first expresion) (sustituir expresion (rest valorAcomprobar) 1))
        )
      )
      (if (eq (listp (first valorAcomprobar)) 'T)
        (list (first (first valorAcomprobar)) (sustituir expresion (rest (first valorAcomprobar)) 1))
        (if (eq (esVariable valorAcomprobar) 'T)
          (first valorAcomprobar)
          (first valorAcomprobar)
        )
      )
    )
  )
)

(defun devolverResto (lista)
  (unless (= (length lista) 1)
    (if (eq (null listaDevuelta) 'T)
      (list first lista)
      (list listaDevuelta (first lista))
    )
    (devolverResto (rest lista))
  )
  (if (eq listaDevuelta 'NIL)
    (list first lista)
    (list listaDevuelta (first lista))
  )
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
