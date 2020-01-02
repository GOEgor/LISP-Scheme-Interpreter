# lisp-parser
Initial implementation of Lisp (Scheme) interpreter.
Only REPL is done so far. 

Done: define, lambdas, quotes, unquotes, quasiquotes, conditionals, let, some primitive functions, error handling, basic macros (define-syntax-rule).
ToDo: more error finding?

# Examples:

Factorial: (two ways)     
```lisp
(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
```

Fibonacci: 
```lisp         
(define (fib n) (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
```

Errors:         
```lisp
>(define (f x) x)  
>(f 1 2 3) => WrongNumberOfArgs "f"
```

Quasiquotations:      
```lisp
`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f) => (a `(b ,(+ 1 2) ,(foo 4 d) e) f)
 ```

With let:       
```lisp
(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e)) => (a `(b ,x ,'y d) e)
```

Macro example:
```lisp
(define-syntax-rule (ifcond condition iftrue iffalse) `(cond (,condition ,iftrue) (else ,iffalse)))
```

Quine example:
```lisp
((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x))))
```
