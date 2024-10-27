((lambda (x y) (+ x y)) 2)
((lambda (x y) (+ x y)) 2 3)
((lambda (f) (f x)) (lambda (x) (+ x 2)))
((lambda (f) (lambda (x) (f x))) (lambda (x) (+ x 1)) 3)
