

(defun belongs (x l)
  (cond
    ((null l      ) nil)
    ((eq x (car l))   t)
    (t      (belongs x (cdr l)))
  ) 
) 

(defun lookup (elem l)
  (if (null l) nil
    (if (eq elem (caar l))
      (cadar l)
      (lookup elem (cdr l))
    )
  )
)


(defun expand_env (names vals env)
    (if (null names) env
        (expand_env (cdr names) (cdr vals) (alter_env (car names) (car vals) env))
    )
)
(defun alter_env (name val env)
 (if (belongs name (mapcar 'car env) )  ; pertenece al ambi
     (replace_in_env name val env)  ; pisarla
     (cons (list name val) env)  ; agregarla
 )
)
(trace alter_env)

(defun replace_in_env (name val env)
    (if (eq name (caar env))
      (cons (list (name val)) (cdr env))
      (cons (car env) (replace_in_env name val (cdr env)))
    )
)

(defun isarithmetic (x)
   (belongs x '(+ - * / and or eq))
)


(defun tclapply (fn lea amb)
    (if (atom fn)
        (cond
            ((eq fn 'cons)   (cons (car lea) (cadr lea)))
            ((eq fn  'car)   (caar lea))
            ((eq fn  'cdr)   (cdar lea))
            ((isarithmetic fn)  (apply fn lea))
            ;((isarithmetic fn)  'ARIT)
            ;(t 4)
            ;TODO: aritmetic, not, functional forms, user defined functions
            (t              (tclapply   (lookup fn amb) lea amb))
        )
        ; lambda
        (tcleval    (caddr fn)  (expand_env (cadr fn)  lea amb) )
    )
)

(defun tcleval (exp amb)
    (if (null exp) 
        nil
        ; Expresiones atómicas
        (if (atom exp)
                ; Numericas o definidas en el enve
            (if (numberp exp)  exp (lookup exp amb))
 	    
		    ; Expresiones no atómicas
	            (cond 
	                ((eq (car exp) 'quote)  (cadr exp))
	                ((eq (car exp) 'if)     (if (tcleval  (cadr exp)  amb)
	                                            (tcleval  (caddr exp) amb)
	                                            (tcleval  (caddr exp) amb)
	                                        )
	                )

	                ; TODO or
	                ;(cond nil)
	                ((eq (car exp) 'lambda) exp)
	                ((eq (car exp) 'and )   (if (null (tcleval (cadr exp) amb))
	                                            nil
	                                            (tcleval (caddr exp) amb)
	                                        )
	                )
	                ((eq (car exp)   'or)    (if (null (tcleval (cadr exp) amb))
	                                            (tcleval (caddr) amb)
	                                            t
	                                        )
                    )
                    
                    ; car, cdr
                    ;((belongs (car exp) '(car cdr))  (apply (car exp) (cdr exp)))
	                (t    (tclapply  (car exp)  (mapcar (lambda (x) (tcleval x amb)) (cdr exp)) amb ))
	            )

            ) ; fin ifatom
        
        ) ; fin ifnull
    
) ; fin defun

(trace tcleval)
(trace tclapply)
(trace belongs)
(trace expand_env)
;(print (tcleval '(+ 2 1) nil))
(setq miamb  '(
                (foo   5)
                (mivar 7)
                (bar   A)
                (fun    (lambda (x y) (+ (+ x 5) y)))
                (milist (a b c d e))
              )
)

;prueba literal
;(print (eq (tcleval 'foo miamb)  5 ))
;prueba suma literal y variable
;(print (eq     (tcleval '(+ 1 'mivar) miamb) 8))
;prueba funcion definida en el ambiente
;(print (eq (tcleval '(fun 4 7) miamb) 16))
;prueba car
;(print (eq (tcleval '(car '(i j k)) miamb) 'i))
;prueba cadr
;(print (eq (tcleval '(car (cdr '(i j k))) miamb) 'j))







