((lambda (x) (x x)) (lambda (x) (x x)))

(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))

((lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x))))) f)

(let (Y (lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))) (Y f))
