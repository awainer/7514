

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


; TODO
(defun expand_ambient nil)



(defun isarithmetic (x)
   (belongs x '(+ - * / and or eq))
)


(defun tclapply (fn lea amb)
    (if (atom fn)
        (cond
            ((eq fn 'cons)   (cons (car lea) (cadr lea)))
            ((eq fn  'car)   (caar lea))
            ((isarithmetic fn)  (apply fn lea))
            ;((isarithmetic fn)  'ARIT)
            ;(t 4)
            ;TODO: aritmetic, not, functional forms, user defined functions
            (t              (tclapply   (lookup fn amb) lea amb))
        )
        ; lambda
        (tcleval    (caddr fn)  (expand_ambient (cadr fn)  lea amb) )
    )
)

(defun tcleval (exp amb)
    (if (null exp) 
        nil
        ; Expresiones atómicas
        (if (atom exp)
                ; Numericas o definidas en el ambiente
            (if (numberp exp)  exp (lookup exp amb))
 	    
		    ; Expresiones no atómicas
	            (cond 
	                ((eq (car exp) 'quote)  (tcleval (cadr exp) amb))
	                ((eq (car exp) 'if)     (if (tcleval  (cadr exp)  amb)
	                                            (tcleval  (caddr exp) amb)
	                                            (tcleval  (caddr exp) amb)
	                                        )
	                )

                	;((eq fn 'not)           (not (tcleval lea amb)))   
	                ; TODO
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
	                (t    (tclapply  (car exp)  (mapcar (lambda (x) (tcleval x amb)) (cdr exp)) amb ))
	            )

            ) ; fin ifatom
        
        ) ; fin ifnull
    
) ; fin defun

; Un literal
;(print (tcleval 1 nil))
;Una suma entre literales
;Una suma entre un literal y una variable
(trace tcleval)
(trace tclapply)
(trace belongs)
(print (tcleval '(+ 2 1) nil))
;(print
;    (tcleval
;        '(+ 1 'mivar)
;        '( (mivar 8) )
;    )
;)






