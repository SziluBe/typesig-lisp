#Y:
(lambda (f)((lambda (i) (i i))(lambda (i)(f (lambda (x)((i i) x))))))
#F:
(lambda (f) (lambda (n) (if n (* n (f (- n 1))) 1)))
#Y F 2:
((lambda (f)((lambda (i) (i i))(lambda (i)(f (lambda (x)((i i) x)))))) (lambda (f) (lambda (n) (if n (* n (f (- n 1))) 1))) 2)
