# Y: (lambda (f) ((lambda (x) (f x x)) (lambda (x) (f x x))))
# F: (lambda (f) (lambda (x) (if x (x * f (- x 1)) 1)))

# 5 Factorial; YF 5:
((lambda (f) ((lambda (x) (f x x)) (lambda (x) (f x x)))) (lambda (f) (lambda (x) (if x (x * f (- x 1)) 1))) 5)
