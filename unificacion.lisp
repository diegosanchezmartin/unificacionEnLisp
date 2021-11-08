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
  (setq z1 (unificacion f1 f2))
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
  (if (eq z1 'NIL)
    (setq g2 t2)
    (setq g2 (sustituir z1 t2 0))
  )
  (setq z2 (unificacion g1 g2))
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
)

(defun sustituir (expresion valorAcomprobar bandera)
  (unless (null expresion)
    (setq valor (first (last expresion)))
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
