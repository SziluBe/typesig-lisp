test "((lambda (x y) (+ x y)) 2)"
test "((lambda (x y) (+ x y)) 2 3)"
test "((lambda (f) (f x)) (lambda (x) (+ x 2)))"
test "((lambda (f) (lambda (x) (f x))) (lambda (x) (+ x 1)) 3)"

test "(if 0 0 1)"
test "(if 1 1 0)"

test "((lambda (f)((lambda (i) (i i))(lambda (i)(f (lambda (x)((i i) x)))))) (lambda (f) (lambda (n) (if n (* n (f (- n 1))) 1))) 2)"
test "((lambda (f)((lambda (i) (i i))(lambda (i)(f (lambda (x)((i i) x)))))) (lambda (f) (lambda (n) (if n (* n (f (- n 1))) 1))) 3)"
