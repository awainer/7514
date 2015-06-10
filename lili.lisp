

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

(defun replace_in_env (name val env)
    (if (eq name (caar env))
      (cons (list (name val)) (cdr env))
      (cons (car env) (replace_in_env name val (cdr env)))
    )
)

(defun isarithmetic (x)
   (belongs x '(+ - * / and or eq))
)

(defun iscontruct (x)
   (belongs x '(cons list append))
)


(defun tclapply (fn lea amb)
    (if (atom fn)
        (cond
            ((eq fn  'car)      (caar lea))
            ((eq fn  'cdr)      (cdar lea))
            ((iscontruct   fn)  (apply fn   lea))
            ((isarithmetic fn)  (apply fn   lea))
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

	                ((eq (car exp) 'lambda) exp)
	                ((eq (car exp) 'and )   (if (null (tcleval (cadr exp) amb))
	                                            nil
	                                            (tcleval (caddr exp) amb)
	                                        )
	                )
	                ((eq (car exp)   'or)    (if (null (tcleval (cadr exp) amb))
	                                            (tcleval (caddr exp) amb)
	                                            t
	                                        )
                    )
                    ((eq (car exp)  'not)   (not (tcleval (cdr exp) amb)))
                    ;TODO
                    ((eq (car exp) 'cond)    nil)
                    ; Formas funcionales  - TODO
                    ((eq (car exp) 'mapcar)     nil)
                    ((eq (car exp) 'reduce)     nil)
                    ((eq (car exp) 'apply)     nil)

	                (t    (tclapply  (car exp)  (mapcar (lambda (x) (tcleval x amb)) (cdr exp)) amb ))
	            )

            ) ; fin ifatom
        
        ) ; fin ifnull
    
) ; fin defun

(defun eqlist (l1 l2)
  (cond
     ((and (null l1) (null l2)) t)
     ((null l1) nil)
     ((null l2) nil)
     ((eq  (car l1) (car l2))   (eqlist (cdr l1) (cdr l2)))
     (t nil)
  )  
) 

;(trace tcleval)
;(trace tclapply)
;(trace belongs)
;(trace lookup)
;(trace expand_env)
;(trace alter_env)
;(print (tcleval '(+ 2 1) nil))
(setq miamb  '(
                (foo   5)
                (mivar 7)
                (bar   A)
                (fun    (lambda (x y) (+ (+ x 5) y)))
                (milist (a b c d e))
                (unbool nil)
                (otral (7 8 9 10))
                (t    t )
                (nil nil) 
              )
)

;; Tests: estos tests deben devolver T
; prueba literal
;(print (eq (tcleval 'foo miamb)  5 ))
;; prueba suma literal y variable
;(print (eq     (tcleval '(+ 1 mivar) miamb) 8))
; prueba suma de variables
(print (eq (tcleval '(+ foo mivar) miamb) 12))
;; prueba funcion definida en el ambiente
;(print (eq (tcleval '(fun 4 7) miamb) 16))
;;; prueba car
;(print (eq (tcleval '(car '(i j k)) miamb) 'i))
;;; prueba cadr
;(print (eq (tcleval '(car (cdr '(i j k))) miamb) 'j))
;;; prueba car con lista de ambiente
;(print  (eq (tcleval '(car milist) miamb) 'a))
;; prueba or
;(print (tcleval '(or t t) miamb))
;(print (tcleval '(or t unbool) miamb))
;(print (tcleval '(not (or nil unbool)) miamb))
;; prueba constructores
(print (eqlist (tcleval '(cons foo milist) miamb) '(5 a b c d e)  ))
(print (eqlist (tcleval '(append otral  milist) miamb) '(7 8 9 10 a b c d e)  ))
(print (eqlist (tcleval '(list foo foo bar) miamb) '(5 5 A) ))


