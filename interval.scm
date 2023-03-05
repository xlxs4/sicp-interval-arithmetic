(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))

(define (mul-interval x y)
  (define (opposite-pair? a b)
    (if (positive? a)
        (negative? b)
        (positive? b)))
  (define (positive-pair? a b)
    (if (opposite-pair? a b)
        #f
        (positive? a))) ; due to our constrcutor
  (define (negative-pair? a b)
    (if (opposite-pair? a b)
        #f
        (negative? a)))
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((negative-pair? xl xu)
           (cond ((opposite-pair? yl yu)
                  (make-interval (* xl yl) (* xl yu)))
                 ((negative-pair? yl yu)
                  (make-interval (* xu yu) (* xl yl)))
                 (else
                  (make-interval (* xu yl) (* xl yu)))))
          ((positive-pair? xl xu)
           (cond ((opposite-pair? yl yu)
                  (make-interval (* xu yl) (* xu yu)))
                 ((negative-pair? yl yu)
                  (make-interval (* xu yl) (* xl yu)))
                 (else
                  (make-interval (* xl yl) (* xu yu)))))
          (else
           (cond ((positive-pair? yl yu)
                  (make-interval (* xl yu) (* xu yu)))
                 ((negative-pair? yl yu)
                  (make-interval (* xu yl) (* xl yl)))
                 (else
                  (make-interval (min (* xl yu) (* xu yl))
                                 (max (* xl yl) (* xu yu)))))))))

(define (div-interval x y)
  (let ((yl (lower-bound y))
        (yu (upper-bound y)))
    (if (<= 0 (* yl yu))
        (error "Division error (interval spans 0)" y)
        (mul-interval x
                      (make-interval
                       (/ 1.0 yu)
                       (/ 1.0 yl))))))
