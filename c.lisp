(defun belongs (x l)
  (cond
    ((null l      ) nil)
    ((eq x (car l))   t)
    (t      (belongs x (cdr l)))
  ) 
) 



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

(defun tobool (x) (not (eq 0 x)))

(defun esoperador (x)
    (belongs x  '(+ - * / < > <= >= ++ -- || &&) )
)

(defun operar (operador operando1 operando2)
    (cond
        ((belongs operador '(+ - * /))  (apply operador (list operando1 operando2)))
        (t nil) ;TODO 
    )
)

(defun peso (x)
    (case x 
        ('+ 2)
        ('- 2)
        ('* 3)
        ('/ 3)
    )
)
;(print (operar '* 2 3))

(defun evaluar (expr mem &optional (operadores nil) (operandos nil))
    (if (esoperador (car expr))
        ;vino un operador y no tengo ninguno
        (if (null operadores)
            (evaluar mem (cdr expr)  (cons (car expr) operadores) operandos)
            ;vino un operador pero ya tenia uno
            (if (> (peso (car expr)) (peso (cdr operadores)))
                ;la que viene tiene mayor prioridad que la que estab en la pila
                (evaluar mem (cdr expr)  (cons (car expr) operadores) operandos)
                ;la que viene tiene menor prioridad
            )
        )
        
        ; No es un operador, lo apilo
        (evaluar mem (cdr expr)  operadores (cons (car expr) operandos))
    )
)

(trace evaluar)
(evaluar nil '( 2 + 3 * 4))
;(defun evaluar (exp mem &optional (operadores nil) (operandos nil))
;   (if (null exp)
;     (if (null operadores) 
;       (car operandos) ; fin de la expresion
;       (evaluar 
;         nil 
;         mem 
;         (cdr operadores)
;         (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos))
;       )
;     )
;     ;exp not null
;     (if (esoperador (car exp))
;       (if (null operadores)
;         (evaluar (cdr exp)
;                  mem
;                  (cons (car exp) operadores) 
;                  operandos
;         )
;         (if (> (peso (car exp) (peso car operadores)))
;            (evaluar (cdr exp) mem (cons (car exp) operadores) operandos)
;            (evaluar 
;              nil 
;              mem 
;              (cdr operadores)
;              (cons (operar (car operadores) (cadr operandos) (car operandos)) (cddr operandos))
;            )
;        )
;      )
;      (
;        evaluar operadores (cons (car exp) operandos )
;      )
;    )
;  )
;)

(trace evaluar)
(trace operar)
(print (evaluar '(3 + 4 + 5) nil))



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


            ;((belongs  (cadar pgm) '(+ - * / < > <= >=))    (apply (cadar pgm) 
            ;                                                    (list  (ejec  (list(caar pgm)) ent mem sal)
            ;                                                            (ejec (list(caddar pgm)) ent mem sal))
            ;                                                )
            ;)

            ((eq (caar pgm) 'while)    (if  (ejec (list (cadar pgm)) ent mem sal)
                                         (ejec (append (caddar pgm) pgm) ent mem sal) ; appendeo al cuerpo el bloque del while
                                         (ejec (cdr pgm) ent mem sal)  ; salgo del bloque
                                       )
            )

            ((eq (caar pgm) 'if)       (if  (ejec (list (cadar pgm)) ent mem sal) 
                                            (ejec  (caddar pgm) ent mem sal) 
                                            (ejec (cadr (cdddar pgm)) ent mem sal)
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
              (int a = 2  b =  5)
              (main
                 (if (b < a)  ((printf 0))  else (( printf 1)))
              )
            )
)

(setq ent1 '(0 5 3 6))
(trace lookup)
(trace ejec)
(trace scanf)
(trace run)
;(run pgm2 ent1)
;(run pgm2 nil)
