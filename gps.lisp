(setq grafo '(
              (1 (2))
              (2 (3 8))
              (3 (4))
              (4 (5 10))
              (5 (6 11))
              (6 nil)
              (7 (1 8))
              (8 (9 14))
              (9 (10 3))
              (10 (11 16))
              (11 (12 5 17))
              (12 (6))
              (13 (7))
              (14 (13 20))
              (15 (9 14))
              (16 (15 22))
              (17 (16 11 23))
              (18 (17 12))
              (19 (13 20))
              (20 (21 25))
              (21 (15 22))
              (22 (28 23))
              (23 (24 17 29))
              (24 (18))
              (25 (19))
              (26 (25))
              (27 (21 26))
              (28 (27))
              (29 (28 23))
              (30 (24 29))

             )
)

(setq grafo_chico '(
                    (1 ( 5))
                    (2 (3))
                    (3 (2 5))
                    (4 (3))
                    (5 (4))
                   )
)

(setq  nombre_esquinas '( 
                          (1 (mexico peru)) 
                          (2 (mexico bolivar)) 
                          (3 (mexico defensa)) 
                          (4 (mexico balcarce)) 
                          (5 (mexico paseo_colon)) 
                          (6 (mexico azopardo)) 
                          (7 (chile peru)) 
                          (8 (chile bolivar)) 
                          (9 (chile defensa)) 
                          (10 (chile balcarce)) 
                          (11 (chile paseo_colon)) 
                          (12 (chile azopardo)) 
                          (13 (independencia  peru)) 
                          (14 (independencia  bolivar)) 
                          (15 (independencia  defensa)) 
                          (16 (independencia  balcarce)) 
                          (17 (independencia  paseo_colon)) 
                          (18 (independencia  azopardo)) 
                          (19 (eeuu  peru)) 
                          (20 (eeuu  bolivar)) 
                          (21 (eeuu  defensa)) 
                          (22 (eeuu  balcarce)) 
                          (23 (eeuu  paseo_colon)) 
                          (24 (eeuu  azopardo)) 
                          (25 (carlos_calvo  peru)) 
                          (26 (carlos_calvo  bolivar)) 
                          (27 (carlos_calvo  defensa)) 
                          (28 (carlos_calvo  balcarce)) 
                          (29 (carlos_calvo  paseo_colon)) 
                          (30 (carlos_calvo  azopardo)) 
                         )
)


(defun lookup (elem l)
    (if (null l) nil
          (if (eq elem (caar l))
                  (cdar l)
                        (lookup elem (cdr l))
                            )
            )
)

(defun belongs (x l)
  (cond
    ((null l      ) nil)
    ((eq x (car l))   t)
    (t      (belongs x (cdr l)))
  ) 
)


(defun vecinos (nodo grafo)
    (if (null grafo)
        nil
        (if (eq nodo (caar grafo))
            (cadar grafo)
            (vecinos nodo (cdr grafo))
        )
    )
)
            

(defun elimina_elem (x l)
    (if (null l)
        nil
        (if (eq x (car l))
            (elimina_elem x (cdr l))
            (cons (car l) (elimina_elem x (cdr l)))
        )
    )
)



(defun diferencia (l1 l2)
    (if (null l2)
        l1
        (if (belongs (car l2) l1)
            (diferencia (elimina_elem (car l2) l1) (cdr l2))
            (diferencia l1 (cdr l2))
        )
    )
)

;(print (diferencia '(5 2 9 2 6 3 1 5) '(1 2)))

(defun expand_tray (tray  grafo)
    (if (null (vecinos_sin_visitar  tray grafo))
      (list tray) ; por consistencia, mapcar devuelve lista
        (mapcar
            (lambda (x) (cons x tray))
            (vecinos_sin_visitar  tray grafo)
        )
    )
)
(trace expand_tray)
(defun vecinos_sin_visitar (tray grafo)
        (diferencia (vecinos (car tray) grafo)   tray)
)

(trace vecinos_sin_visitar)

(defun encontre_solucion (f tray)
    (if (null tray)
        nil
        (if (eq f (caar tray))
            t
            (encontre_solucion f (cdr tray))
        )
    )
)
(trace encontre_solucion)

;(defun flat (x) 
;    (if (listp (car x))
;        (if (listp (caar x))
;          (flat (ap
;)

(defun expand_level (tray grafo)
    (reduce 'append
        (mapcar (lambda (x)  (expand_tray x grafo))  tray)
        )
)

(defun puedo_expandir (tray grafo f)
    (reduce (lambda (&optional (x nil) (y nil) ) (or x y))    (mapcar (lambda (x) (not (null (vecinos_sin_visitar x grafo))) ) tray))
)
(trace puedo_expandir)
(trace vecinos_sin_visitar)
(trace expand_level)
;(trace expand_tray)
;(trace puedo_expandir)
(defun gps (i f grafo &optional (tray (list (list i))))
        (if (not (encontre_solucion f tray))
            (if (puedo_expandir tray grafo f)
                (gps i f grafo   (expand_level tray grafo))
                tray
            )
            tray
        )
    
)

(defun eq_esquina (esq1 esq2)
    (or
        (and  (eq (car esq1) (car esq2)) (eq (cadr esq1) (cadr esq2)))
        (and  (eq (car esq1) (cadr esq2)) (eq (cadr esq1) (car esq2)))
    )
)

(defun nodos_a_nombres (nodos dict_nombres)
    (mapcar 'car 
        (mapcar (lambda (x) (lookup x dict_nombres)) nodos)
    )
)

(defun esquina_a_id (esquina  grafo)
    (if (eq_esquina esquina (cadar grafo))
        (caar grafo)
        (esquina_a_id esquina (cdr grafo))
    )
)
(trace gps)
;(trace esquina_a_id)
;(trace eq_esquina)
;(esquina_a_id '(chile bolivar) nombre_esquinas )


(defun gps_lindo (i f grafo)
 (parse_salida_linda
  (salida_linda
    (nodos_a_nombres 
      (elimina_falso_positivo 
        (gps 
          (esquina_a_id i nombre_esquinas)  
          (esquina_a_id f nombre_esquinas)  
          grafo
        ) 
        (esquina_a_id f nombre_esquinas)
      ) 
      nombre_esquinas
    )
   )
 )
)

(defun elimina_falso_positivo (tray f)
    (if (null tray)
        nil
        (if (eq f (caar tray))
            (reverse (car tray))
            (elimina_falso_positivo (cdr tray) f)
        )
    )
)

;(defun misma_calle (esq1 esq2)
;    (or
;        (eq (car esq1) (car esq2))
;        (eq (cdr esq1) (cdr esq2))
;        (eq (car esq1) (cdr esq2))
;        (eq (cdr esq1) (car esc2))
;    )
;)

(defun intersec (e1 e2)
    (cond  
        ((eq (car e1) (car e2))       (car e1))
        ((eq (car e1) (cadr e2))      (car e1))
        (t (cadr e1))
    )
)

;(trace belongs)
;(trace intersec)
(defun salida_linda (tray &optional (sal (list (list (intersec (car tray) (cadr tray)) 0))))
  (if (null tray)
    (reverse sal)
    (if (belongs (caar sal) (car tray))
       (salida_linda (cdr tray)  (cons (list (caar sal)  (+ 1 (cadar sal)))   (cdr sal)))
       (salida_linda (cdr tray)  (cons (list (caar tray)  0)   sal) )
    )
  )
)
(trace salida_linda)

(defun parse_salida_linda (sal)
    (mapcar (lambda (x)
                (append '(Doble por) (list (car x))  '( y avance) (cdr x)  '(cuadras.))
            )
            sal
    )
)

;(trace salida_linda)
;
;(trace gps)
;(trace encontre_solucion)
(trace elimina_falso_positivo)
;(print (gps_lindo 1 2  grafo_chico ))

(print (gps_lindo  '(paseo_colon chile)    '(eeuu defensa)  grafo))
;(print (gps  5 20  grafo))
;(setq unr '((EEUU DEFENSA) (INDEPENDENCIA DEFENSA) (CHILE DEFENSA) (MEXICO DEFENSA) (MEXICO BALCARCE) (MEXICO PASEO_COLON) (MEXICO AZOPARDO)))
;(print (parse_salida_linda (salida_linda unr)))
;(intersec '(EEUU DEFENSA) '(INDEPENDENCIA DEFENSA))
