

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

(defun tclcond (l amb)
    (if (tcleval (caar l) amb)
        (tcleval (cadar l) amb)
        (tclcond  (cdr l) amb)
    )
)

(defun tclapply (fn lea amb)
    (if (atom fn)
        (cond
            ;((eq fn  'car)      (car lea))
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

(defun evallist (l amb)
    (cond ((null l)  nil)
          (t    (cons  (tcleval (car l) amb) (evallist (cdr l) amb)))
    )
)

(defun simplemapcar (fun amb l) nil)
;(defun multimapcar (fun amb l &optional (contador (cons 1 l))) 
(defun multimapcar (fun amb l &optional (contador  l)) 
  (if (null contador) nil
    (cons 
        (tclapply fun   (mapcar 'car  l) amb )
        (multimapcar fun amb (mapcar 'cdr  l) (cdr contador))
    )
  )

)
(trace simplemapcar)
(trace multimapcar)

(defun tclmapcar (funct amb &rest l )
    (if (null (caar l))  nil
      (if  (eq 1 (length   (car l)))
        (simplemapcar funct amb (caar l))
        (multimapcar  funct amb (car l))
      )
    )
)
;        (if (eq (length  (car l)) 1) 
;            ;(cons 
;            ;    (tclapply funct  (mapcar 'car  l) amb )
;            ;    (tclmapcar funct amb  (mapcar 'cdr  l))
;            ;) 
;            (cons (tclapply  funct  (list (caar l)) amb)
;                   ;(cdar l)
;                   (tclmapcar funct amb  (cdar l))
;            )
;       
;            88
;        )
;    )
;    (mapcar (append
;        (lambda (x) (tclapply funct  x amb)))
;        l     
;    ) 
;    (if (null l) nil
;        (cons (tclapply  funct (list(car l))  amb)
;              (tclmapcar funct (cdr l)  amb)
;        )
;    )
;)

(defun tclreduce (funct l amb)
    
    (if (eq 1 (length l)) (car l)
      (tclreduce funct (cons (tclapply  funct (list (car l) (cadr l)) amb) (cddr l)) amb)
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
                        ((eq (car exp)  'not)       (not (tcleval (cdr exp) amb)))
                        ((eq (car exp) 'cond)       (tclcond    (cdr exp) amb))
                        ((eq (car exp) 'mapcar)     (tclmapcar  (cadr exp)  amb  (evallist (cddr exp) amb) ))
                        ((eq (car exp) 'reduce)     (tclreduce  (cadr exp)  (car (evallist (cddr exp) amb))  amb) )
                        ((eq (car exp) 'apply)      (tclapply   (cadr exp)  (cddr exp) amb))

                        (t    (tclapply  (car exp)  (mapcar (lambda (x) (tcleval x amb)) (cdr exp)) amb ))
	            ) ; fin cond

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

(trace tcleval)
(trace tclapply)
(trace evallist)
;(trace belongs)
;(trace lookup)
;(trace expand_env)
;(trace alter_env)
;(trace tclcond)
(trace tclmapcar)
;(trace tclreduce)
;(print (tcleval '(+ 2 1) nil))
(setq miamb  '(
                (foo   5)
                (mivar 7)
                (bar   A)
                (fun    (lambda (x y) (+ (+ x 5) y)))
                (milist (a b c d e))
                (unbool nil)
                (otral (7 8 9 10))
                (masuno  (lambda (x) (+ 1 x)))
                (primero (lambda (x) (car x)))
                (pares  ((1 2) (3 4)))
                (par    (8 9) )
                (t    t )
                (nil nil) 
              )
)


;;; Tests: estos tests deben devolver T
; prueba literal
;(print (eq (tcleval 'foo miamb)  5 ))
;;; prueba suma literal y variable
;(print (eq     (tcleval '(+ 1 mivar) miamb) 8))
;;;; prueba suma de variables
;(print (eq (tcleval '(+ foo mivar) miamb) 12))
;;;; prueba funcion definida en el ambiente
;(print (eq (tcleval '(fun 4 7) miamb) 16))
;;; prueba car
;(print (eq (tcleval '(car '(i j k)) miamb) 'i))
;;;; prueba cadr
;(print (eq (tcleval '(car (cdr '(i j k))) miamb) 'j))
;;;;; prueba car con lista de ambiente
;(print  (eq (tcleval '(car milist) miamb) 'a))
;;;; prueba or
;(print (tcleval '(or t t) miamb))
;(print (tcleval '(or t unbool) miamb))
;(print (tcleval '(not (or nil unbool)) miamb))
;;; prueba constructores
;(print (eqlist (tcleval '(cons foo milist) miamb) '(5 a b c d e)  ))
;(print (eqlist (tcleval '(append otral  milist) miamb) '(7 8 9 10 a b c d e)  ))
;(print (eqlist (tcleval '(list foo foo bar) miamb) '(5 5 A) ))
;;; prueba cond
;(print (eq 0 (tcleval    '(cond 
;                            ((and t unbool) 5)
;                            ((eq  'A bar)   0)
;                            (t              1)
;                          )
;                    
;               miamb
;              )
;       )
;)

;; prueba mapcar
;(print 
;   (eqlist 
;        (tcleval    '(mapcar primero pares) miamb)
;        '(1 3)
;   )
;)
;
;
;(print 
;   (eqlist 
;        (tcleval    '(mapcar car pares) miamb)
;        '(1 3)
;   )
;)
;
;
;(print 
;;   (eqlist
;        (tcleval    '(mapcar car '((a b c) (d e f))) nil)
;;        '(a d)
;;   )
;)

(print
        (tcleval  '(mapcar list  '(a b c) '(d e f)) nil)
;         (tcleval  '(mapcar car   '( (1 2) (3 4) )) nil)


)

;(print         (tcleval  '(mapcar car   '(a b c) '(d e f)) nil))
;; prueba apply
;(print (eq (tcleval '(apply masuno 5) miamb) 6))
;;; test reduce
;(print (eq (tcleval '(reduce +  otral) miamb) 34))
;(print (eq (tcleval '(reduce *  '(2 2 2 2)) miamb) 16))
