; TODO
(defun lookup nil)
(defun expand_ambient nil)

(defun tclapply (fn lea amb)
    (if (atom fn)
        (cond
            ((eq fn 'cons)  (cons (car lea) (cadr lea)))
            ((eq fn  'car)   (caar lea))
            ;TODO: aritmetic, not, functional forms, user defined functions
            (t              (tclapply   (lookup fn amb) lea amb))
        )
    (tcleval    (caddr fn)  (expand_ambient (cadr fn)  lea amb) )
)

(defun tcleval (exp amb)
    (if (null exp) 
        nil
        ; Expresiones atómicas
        (if (atom exp)
            ; Numericas
            (if (numberp exp)
            exp
            (lookup exp amb)
            (cond
                ((eq (car exp) 'quoute) (cadr exp))
                ((eq (car exp) 'if)     (if (tcleval  (cadr exp)  amb)
                                            (tcleval  (caddr exp) amb)
                                            (tcleval  (caddr exp) amb)
                                        )
                )
                ; TODO
                (cond nil)
                ((eq (car exp) 'lambda) exp)
                ((eq (car exp) 'and )   (if (null (tcleval (cadr exp) amb))
                                            nil
                                            (tcleval (caddr exp) amb)
                                        )
                )
                (eq (car exp)   'or)    (if (null (tcleval (cadr exp) amb))
                                            (tcleval (caddr) amb)
                                            t
                                        )
                (t    (tclapply  (car exp)  (mapcar (lambda (x) (tcleval x amb)) (cdr exp))))
            )

            )
        )
    )
)
