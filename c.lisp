
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

(defun scanf (name mem val)
    (cond
        ((null mem)             (list  name val))
        ((eq (car mem) name)    (append (list  name val) (cddr mem)))
        (t                      (cons (car mem) (scanf name (cdr mem) val)))
    )
)

(defun suma nil)
(trace suma)
(defun ejec (pgm ent mem &optional (sal nil))
    (if (null pgm)
      mem ; temp
      (if (atom (car pgm))
        (if (numberp (car pgm))
            (car pgm)
            (lookup (car pgm) mem)
        )
        (cond
            ((eq  (caar pgm) 'scanf)     (ejec   (cdr pgm)   (cdr ent)   (scanf  (cadar pgm) mem (car ent)) sal))
            ((eq  (cadar pgm) '=    )    (ejec   (cdr pgm)   ent         (scanf  (caar pgm)  
                                                                                 mem 
                                                                                 (ejec (list (cddar pgm)) ent mem sal) 
                                                                         )
                                          sal)
            )


            ((eq  (cadar pgm) '+  )    ( + (ejec  (list(caar pgm)) ent mem sal)
                                           (ejec  (list(caddar pgm)) ent mem sal)))
                                         
            (t  mem)
          )
       )
    )
)




(setq pgm1 '(
              (int b)
              (main
                    (scanf a)
                    (b = 3 + a)
              )
            )
)

(setq pgm2 '(
              (int a b = 5)
              (main
                (
                    (scanf a)
                    (b = b + a)
                    (printf a + b)
                    (while (a < b)
                           (  (a ++)
                              (printf a)
                           )
                    )
                    ;(if (a > b)   ()  else () )
                )
              )
            )
)

(setq ent1 '(6 5 3 6))
(trace lookup)
(trace ejec)
(trace scanf)
(trace run)
(run pgm1 ent1)
