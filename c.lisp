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
    ;inicializo en 0
    (t                  (agregar (cdr defs)   (append  
                                                (list  (car defs) 0)  
                                                mem 
                                              )
                        )
    )
  )
)

(defun tobool (x) (not (eq 0 x)))
(defun toint  (x) 
    (if (numberp x) 
        x
       (if x 1 0)
    )
)

(defun esoperador (x)
    (belongs x  '(+ - * / < > <= >= ++ -- || && != == %) )
)

(defun operar (operador operando1 operando2)
    (cond
        ((belongs operador '(+ - * / < > <= >=))  (apply operador (list operando1 operando2)))

        ((eq operador '&&)      (and (tobool operando1) (tobool operando2)))
        ((eq operador '||)      (or  (tobool operando1) (tobool operando2)))
        ((eq operador '%)       (mod operando1 operando2))
        ((eq operador '!=)      (not (eq operando1 operando2)))
        ((eq operador '==)      (eq operando1 operando2))
        (t nil) ;TODO 
    )
)
; http://www.difranco.net/compsci/C_Operator_Precedence_Table.htm
(defun peso (x)
    (case x 
        ('* 6)
        ('/ 6)
        ('% 6) 
        ('+ 5)
        ('- 5)
        ('>  4) 
        ('>= 4) 
        ('<  4) 
        ('<= 4) 
        ('!= 3)
        ('== 3)
        ('&& 2)
        ('|| 1)
    )
)

(defun evaluar (expr mem &optional (operadores nil) (operandos nil))
    (if  (null expr) 
        (if (null operadores) 
            (car operandos)
            (evaluar nil mem (cdr operadores) (cons (toint (operar (car operadores) (cadr operandos) (car operandos))) (cddr operandos) ))
        ) 
        (if (esoperador (car expr))
            ;vino un operador y no tengo ninguno
            (if (null operadores)
                (evaluar  (cdr expr) mem  (cons (car expr) operadores) operandos)
                ;vino un operador pero ya tenia uno
                (if (> (peso (car expr)) (peso (car operadores)))
                    ;la que viene tiene mayor prioridad que la que estab en la pila
                    (evaluar  (cdr expr) mem  (cons (car expr) operadores) operandos)
                    ;la que viene tiene menor prioridad
                    (evaluar expr mem (cddr operadores)  (cons (toint (operar (car operadores) (cadr operandos) (car operandos))) (cddr operandos)  ))
                )
            )
            
            ; No es un operador, lo apilo
            (evaluar  (cdr expr) mem  operadores (cons (car expr) operandos))
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

(defun resolve_exp_vars (expr mem)
    (if (null expr) 
        nil
        (if (or (numberp (car expr)) (esoperador (car expr)))
                  (cons (car expr) (resolve_exp_vars (cdr expr) mem))
                  (cons (lookup (car expr) mem) (resolve_exp_vars (cdr expr) mem))
        )
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
                                                                        (ejec (cadar pgm) ent mem)
                                                                     sal
                                                                    )
                                         )
            )
                                    
            ((eq  (cadar pgm) '=    )    (ejec   (cdr pgm)   ent         (scanf  (caar pgm)  
                                                                                 mem 
                                                                                 (evaluar (resolve_exp_vars (cddar pgm) mem ) mem)
                                                                         )
                                          sal)
            )


            ;((belongs  (cadar pgm) '(+ - * / < > <= >=))    (apply (cadar pgm) 
            ;                                                    (list  (ejec  (list(caar pgm)) ent mem sal)
            ;                                                            (ejec (list(caddar pgm)) ent mem sal))
            ;                                                )
            ;)

            ((eq (caar pgm) 'while)    (if  (tobool (evaluar (resolve_exp_vars  (cadar pgm) mem)  mem))
                                         (ejec (append (cddar pgm) pgm) ent mem sal) ; appendeo al cuerpo el bloque del while
                                         (ejec (cdr pgm) ent mem sal)  ; salgo del bloque
                                       )
            )

            ((eq (caar pgm) 'if)       (if  (tobool (evaluar  (resolve_exp_vars (cadar pgm)mem) mem))
                                            (ejec (append (caddar pgm)  (cdr  pgm)) ent mem sal)
                                            (ejec (cadr (cdddar pgm)) ent mem sal)
                                            ;TODO else
                                       )
            )
            ;TODO autoincrement                                    
            (t  (evaluar (resolve_exp_vars  (car pgm) mem) mem))
          )
       )
    )
)




;(setq pgm1 '(
;              (int b a )
;              (main
;                    (scanf  b)
;               ;     (printf b)
;                    (while (b < 5)
;                        ( (b = b + 1)
;                          (printf b)
;                        )
;                    )
;                    (printf ( 1 + 1 ))
;              )
;            )
;)
;
;(setq pgm2 '(
;              (int a = 2  b =  5)
;              (main
;                 (if (b < a)  ((printf 0))  else (( printf 1)))
;              )
;            )
;)

;(trace lookup)
(trace ejec)
(trace operar)
;(trace scanf)
(trace evaluar)
;(trace resolve_exp_vars)
;(trace run)

(setq fact '(
                (int a = 4 f = 1)
                (main
                    (scanf a)
                    (while (a != 1)
                        (f = f * a)
                        (a = a - 1)
                        ;(if (a % 2 == 0)
                        (if (a > 3)
                            (
                            (printf (a))
                            )
                        )
                    )
                )
            )
)
(print (run fact '(6)))


;(setq ent1 '(0 5 3 6))
;(print (eq 11 (evaluar '(  3 + 4 * 2) nil)))
;(trace evaluar)
;(trace operar)
;(print (eq (t (evaluar '(1 && 4) nil))))
;(evaluar '(2 == 3) nil)
;(run pgm2 ent1)
;(run pgm2 nil)
