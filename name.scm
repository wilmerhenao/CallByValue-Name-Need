;;; Metacircular Evaluator
;;; Adapted from Structure and Interpretation of Computer Programs
;;; http://mitpress.mit.edu/sicp/code/index.html
;; The Core Evaluator: evaluate an expression in an environment

(define (mceval exp env)
;; basically this function is here to find out what the hell is the expression that I am evaluating.
;; INPUT: an expresion and the environment I'm working on
;; OUTPUT: returns an expression (and sometimes the environment)
  (cond ((self-evaluating? exp) exp)   ;;TRUE if boolean, number of string
        ((quoted? exp) (quoted-text exp))   ;;Check if it's a quoted list and return the cadr
        ((symbol? exp) (lookup-variable-value exp env)) ;; access the value o fthe variable
        ((lambda? exp)  ;; basically if this is a lambda expression, make-procedure will make a list
         (make-procedure (lambda-parameters exp)  ;; the arguments of the lambda function
                         (lambda-body exp) ;; the function itself
                         env)) ;; create lambda procedure usando los argumentos y el cuerpo de la funcion
        ((definition? exp) (eval-definition exp env))  ;; Checks if the expression is of the type (define (blah blah blah... and defines the variable
        ((assignment? exp) (eval-assignment exp env)) ;; if this an expression of type set!.  Then do the binding of the variable
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env)) ;; Evaluate the expressions one by one in the list
        ((if? exp) (eval-if exp env)) ;; just an if expression
        ((cond? exp) (mceval (cond->if exp) env)) ;; convert a <cond> type of expression into an <if> type of expression
	;;;;;;;;;;THIS IS THE PART MENTIONED IN THE HINT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ((application? exp)  ;; applications are "other" expressions.  Like (fact 3)
	 (eval-application (secondary-evaluation (car exp) env) (cdr exp) env)) ;; the operator.  Example: in (+ 1 2) is the '+' symbol
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(else (error "unknown expression type" exp))))

;; Evaluation of Individual Expressions

(define (eval-definition exp env)
  (define-variable! (definition-variable exp) ;; here i take the name of the variable
                    (mceval (definition-value exp) env) ;; and here I hold the value of the variable
                    env)
  'ok)

(define (eval-assignment exp env) ;; Bind the value to the expression in this environment
  (set-variable-value! (assignment-variable exp)
                       (mceval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-application procedure arguments env) ;; if primitive procedure then apply.  If compound then  evaluate the sequence one by one.
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure (primitive-flag-eval arguments env))) ;; Notice that I change the function here so that I can evaluate flagged expressions
        ((compound-procedure? procedure)
         (eval-sequence ;; send the whole procedure one by one and split the whole thing until only primitive procedures are left
           (procedure-body procedure) ;; this will be the text in the function ((begin (null? x) (null? x) (null? x) 0))
           (extend-environment
             (procedure-parameters procedure)
             (delay-arguments arguments env)  ;; this part changes so that the arguments do not get yet evaluated but wait until you arrive to primitive procedures
             (procedure-environment procedure))))
        (else (error "unknown procedure type" procedure))))

(define (eval-sequence exps env)  ;; evaluate the whole list passed in exps
  (cond ((null? (cdr exps)) (mceval (car exps) env))
	(else (mceval (car exps) env)
	      (eval-sequence (cdr exps) env))))

(define (eval-if exp env)
  (if (mceval (if-predicate exp) env)
      (mceval (if-consequent exp) env)
      (mceval (if-alternative exp) env)))

;; Self-Evaluating Expressions

(define (self-evaluating? exp)
  (cond ((boolean? exp) #t)
	((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;; Quoted Expressions

(define (quoted? exp) (tagged-list? exp 'quote))
(define (quoted-text exp) (cadr exp))

;; Lambda Expressions and Compound Procedures

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp)) ;; pass the second parameter in the list
(define (lambda-body exp) (cddr exp)) ;; pass the rest of parameters in the list (the BODY of the function)
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))  ;; create a lambda expression

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

;; Definitions

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp) ;; Sets expressions of the form (define (<var> <parameter1> ... <parametern>) <body>)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)  ;; this one creates lambda expresions of the type (define <var>  (lambda (<parameter1> ... <parametern>)    <body>)) syntactic sugar for lambda functions
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;; Assignments

(define (assignment? exp) (tagged-list? exp 'set!)) ;; es una expresion de tipo set?
(define (assignment-variable exp) (cadr exp)) ;; Este es el nombre de la variable
(define (assignment-value exp) (caddr exp)) ;; este es el valor de la variable

;; Applications

(define (application? exp) (pair? exp)) ;; A procedure application is any compound expression that is not one of the others. The car of the expression is the operator, and the cdr is the list of operands
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; Sequences

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq) (cons 'begin seq))

;; Conditionals

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (if (not (null? (cdddr exp))) (cadddr exp) #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp))) ;; transforms cond expressions into "if" type of expressions

(define (expand-clauses clauses)
  (if (null? clauses)
      #f
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "cond else clause isn't last" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((null? (cdr seq)) (car seq))
	(else (make-begin seq))))

;; Environments

(define (first-frame env) (car env))
(define (next-frame env) (cdr env))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (make-frame variables values) (cons variables values))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "extend-environment: too many arguments" vars vals)
          (error "extend-environment: too few arguments" vars vals))))

(define (lookup-variable-value var env)
;; accesses the values of variables.  Returns the value that is bound to the symbol <var> in the environment <env>, or signals an error if the variable is unbound
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (next-frame env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env) ;; changes the binding of the variable <var> in the environment <env> so that the variable is now bound to the value <value>, or signals an error if the variable is unbound. (CHANGES THE VALUE!!!)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (next-frame env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env) ;; adds to the first frame in the environment <env> a new binding that associates the variable <var> with the value <value> (CREATES!)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Primitive Procedures

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (apply-primitive-procedure proc args)
  (apply (cadr proc) args))

(define primitive-procedures
  (list (list 'null? null?)
	(list 'cons cons)
	(list 'car car)
	(list 'cdr cdr)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '= =)
	(list 'display display)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-values)
  (map (lambda (b) (list 'primitive (cadr b))) primitive-procedures))

;; The Empty and the Global Environments

(define the-empty-environment '())

(define (setup-global-environment)
  (let ((env (extend-environment (primitive-procedure-names)
				 (primitive-procedure-values)
				 the-empty-environment)))
    env))

(define the-global-environment (setup-global-environment))

;; Helpers

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; The Read-Eval-Reply Loop

(define (mcloop)
  (display "\nbNAME> ")
  (let ((input (read)))
    (if (equal? input 'exit)
	"Happy happy joy joy"
	(begin (mcprint (secondary-evaluation input the-global-environment))
	       (mcloop)))))

(define (mcprint object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)))
      (display object)))

;; The new stuff

(define (secondary-evaluation exp env)
  (eval-filter (mceval exp env)))

(define pass-val (lambda (x)(x)))

(define (eval-filter objec)
  (cond ((tagged-list? objec 'marked) 
	 (cadr objec))
	((tagged-list? objec 'myflag)
         (let ((my-return (secondary-evaluation (cadr objec) (caddr objec))))
           ;(set-car! objec 'marked) (set-car! (cdr objec) my-return)  ; replace exp with its value
           my-return))
        (else objec)))
;(define (eval-filter objec)
 ; (cond ((tagged-list? objec 'marked) 
;	 (cadr objec))
;	((tagged-list? objec 'primitive)
;	 (pass-val object)
;	 ((tagged-list? objec 'if)
;	 (pass-val object))
;	 ((tagged-list? objec 'procedure)
;	  (pass-val object)
;	((tagged-list? objec 'myflag)
 ;        (let ((my-return (secondary-evaluation (cadr objec) (caddr objec))))
  ;         (set-car! objec 'marked) (set-car! (cdr objec) my-return)
;	   my-return))))))

(define (primitive-flag-eval exp env)
  (if (null? exp) '()
      (cons (secondary-evaluation (car exp) env)  ;; if this is a flagged object then evaluate, if not, just pass it on without doing anything to it
            (primitive-flag-eval (cdr exp) env))))

(define (delay-arguments exp env)
  (if (null? exp) '()
      (cons (list 'myflag (car exp) env) ;; add a flag to the list (just like it was done with primitives)
	    (delay-arguments (cdr exp) env))))

;; esto solo es para el ejemplo

'METACIRCULAR-EVALUATOR-LOADED
