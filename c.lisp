
(defun agregar (defs mem)
  (cond
    ((null defs) mem)
     ; es inicializacion
    ((eq (cadr defs) '=)  (agregar (cdddr defs) 
                                   (append (list (car defs) (caddr defs)) mem)
                                   )
    )
    ;inicializo en nil
    (t                  (agregar (cdr defs)   (append  
                                                (list  (car defs) nil)  
                                                mem 
                                              )
                        )
    )
  )
)

(defun run (pgm ent &optional (mem nil))
  (if   (null pgm)  nil
    (if (eq (caar pgm) 'int)    
          (run  (cdr pgm)  ent (agregar (cdar pgm) mem))
          (if (eq (caar pgm) 'main)
            (ejec (cdar pgm) ent mem)
            (print  pgm)
          )
    )
  )
)


(defun lookup (var mem)
    (cond
        ((eq var (car mem))     (cadr mem))
        (t                      (lookup var (cdr mem)))
    )
)

(defun belongs (x l)
  (cond
    ((null l      ) nil)
    ((eq x (car l))   t)
    (t      (belongs x (cdr l)))
  ) 
) 

(defun scanf (name mem val)
    (cond
        ((null mem)             (list  name val))
        ((eq (car mem) name)    (append (list  name val) (cddr mem)))
        (t                      (cons (car mem) (scanf name (cdr mem) val)))
    )
)

(defun suma nil)
(trace suma)
;(trace while)

(defun ejec (pgm ent mem &optional (sal nil))
    (if (null pgm)
      (reverse sal)
      (if (atom (car pgm))
        (if (numberp (car pgm))
            (car pgm)
            (lookup (car pgm) mem)
        )
        (cond
            ((eq  (caar pgm) 'scanf)     (ejec (cdr pgm) (cdr ent)   (scanf  (cadar pgm) mem (car ent)) sal))
            ((eq  (caar pgm) 'printf)    (ejec (cdr pgm)  ent  mem  (cons
                                                                        (ejec (cdar pgm) ent mem)
                                                                     sal
                                                                    )
                                         )
            )
                                    
            ((eq  (cadar pgm) '=    )    (ejec   (cdr pgm)   ent         (scanf  (caar pgm)  
                                                                                 mem 
                                                                                 (ejec (list (cddar pgm)) ent mem sal) 
                                                                         )
                                          sal)
            )


            ((belongs  (cadar pgm) '(+ - * / < > <= >=))    (apply (cadar pgm) 
                                                                (list  (ejec  (list(caar pgm)) ent mem sal)
                                                                        (ejec (list(caddar pgm)) ent mem sal))
                                                            )
            )

            ((eq (caar pgm) 'while)    (if  (ejec (list (cadar pgm)) ent mem sal)
                                         (ejec (append (caddar pgm) pgm) ent mem sal) ; appendeo al cuerpo el bloque del while
                                         (ejec (cdr pgm) ent mem sal)  ; salgo del bloque
                                       )
            )
                                         
            (t  sal)
          )
       )
    )
)




(setq pgm1 '(
              (int b a )
              (main
                    (scanf  b)
               ;     (printf b)
                    (while (b < 5)
                        ( (b = b + 1)
                          (printf b)
                        )
                    )
                    (printf ( 1 + 1 ))
              )
            )
)

(setq pgm2 '(
              (int a 3  b  5)
              (main
                (
                 (if (b < a)  ( 
                       (printf 0)  
                 )  else ( 
                     ( printf 1) 
                    )
                 )
                )
              )
            )
)

(setq ent1 '(0 5 3 6))
(trace lookup)
(trace ejec)
(trace scanf)
(trace run)
;(run pgm2 ent1)
(run pgm2 nil)
