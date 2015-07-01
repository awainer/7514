(setq grafo '(
              (1 (2))
              (2 (3 8))
              (3 (4))
              (4 (5 10))
              (5 (6))
              (6 nil)
              (7 (1 8))
              (8 (9 14))
              (9 (3 10))
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

(defun belongs (x l)
  (cond
    ((null l      ) nil)
    ((eq x (car l))   t)
    (t      (belongs x (cdr l)))
  ) 
)

;(trace belongs)

;(defun vecinos (nodo grafo &optional (res nil) ) 
;    (if (null grafo)
;        res
;        (if (belongs nodo (cadar grafo))
;            (vecinos nodo (cdr grafo) (cons (caar grafo) res))
;            (vecinos nodo (cdr grafo)   res)
;        )
;    )
;)

(defun vecinos (nodo grafo)
    (if (null grafo)
        nil
        (if (eq nodo (caar grafo))
            (cadar grafo)
            (vecinos nodo (cdr grafo))
        )
    )
)
            
;(trace vecinos)
;(print (vecinos 29 grafo))

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
    (mapcar
        (lambda (x) (cons x tray))
        (vecinos_sin_visitar tray grafo)
    )
)

(defun vecinos_sin_visitar (tray grafo)
        (diferencia (vecinos (car tray) grafo)   tray)
)

(trace expand_tray)

(defun gps (i f grafo &optional (tray (list (list i))))
nil)
;(defun gps (i f grafo &optional (tray (list (list i))))
;
;    (mapcar
;     (lambda (x) (gps i f grafo (expand_tray x grafo)))
;     tray   
;    )     
;    (if (or (eq f (caar tray)) (null (vecinos_sin_visitar ( ))  )
    
;        (cons (car tray) (gps i f grafo (cdr tray)))
        ;(cons (car tray) (gps i f grafo (cdr tray)))
;        (gps i f grafo
;            (append 
;                (expand_tray (car tray) grafo)
;                (cdr tray)
;            )
;        )
;    )
;    (if (null tray)
;      nil
;    )
)
;      (if (eq (caar tray) f)
;        (reverse tray)
;
;         (gps i f grafo
;         (mapcar 
;              (lambda (x) (expand_tray x grafo))
;              tray
;         )
;         )
;        (mapcar (lambda (x) 
;                    ;(append  (vecinos (car x) grafo) x)        
;                       (mapcar 'list
;                            (diferencia  (vecinos    (car x) grafo)
;                                         x
;                            )
;                            x
;                       )
;                )
;                tray
;        )

            
       ;(vecinos (caar tray) grafo)
       ; (gps i f grafo
       ;      (append
             ;  (mapcar (lambda (x) (cons
             ;          )
                      ; (diferencia  (vecinos    (caar tray) grafo)
                      ;              (car tray)
                      ; )
             ;  )
       ;      )
       ; )
;     )
;   )
;)

(trace gps)
(print (gps 10 21 grafo))
;(EXPAND_TRAY (car (EXPAND_TRAY  '(16 10)  grafo)) grafo)

