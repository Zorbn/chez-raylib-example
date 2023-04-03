(library (matrix)
  (export
   make-mat mat-square? mat-square?? make-element mat-set! mat-ref mat-rows mat-cols mat-shape mat? mat?? mat-fill! mat-slice mat-row mat-col mat-copy mat-paste mat-paste! mat-print list->mat
   vector-cat vector-rotate list-rotate vector-mul vector-dot vector-cross vector-reform vector-axis vector-norm vector->mat vector-outer mat->vector vector-ops vector-add vector-scale
   mat-reform mat-scale mat-add mat-ops mat-mmul mat-mul mat-dot mat-trans pi deg->rad rad->deg
   make-vecq vecq-mul vecq-conj vecq-norm2 vecq-inv vecq-exp vecq-log vecq-rot)
  (import (scheme))
  (define make-mat
    (case-lambda
      [(rows cols)
       (make-mat rows cols 0)]
      [(rows)
       (make-mat rows rows)]
      [(rows cols val)
       (do ([m (make-vector rows)]
            [i 0 (+ i 1)])
           ((= i rows) m)
         (vector-set! m i (make-vector cols val)))]))
  
  (define mat-square?
    (lambda (m)
      (and (mat? m)
           (= (mat-rows m) (mat-cols m)))))
  
  (define mat-square??
    (lambda (m)
      (and (mat?? m)
           (= (mat-rows m) (mat-cols m)))))
  
  (define make-element
    (lambda (m)
      (if (integer? m)
          (do ([mat (make-vector m)]
               [i 0 (+ i 1)])
              ((= i m) mat)
            (vector-set! mat i
                         (let ([v (make-vector m)])
                           (vector-set! v i 1) v)))
          (if (mat-square? m)
              (make-element (mat-cols m))
              (raise
               (condition
                (make-error)
                (make-message-condition "Not a square mat")))))))
  
  (define mat-set!
    (lambda (m row col val)
      (vector-set! (vector-ref m row) col val)
      m))
  
  (define mat-ref
    (lambda (m row col)
      (vector-ref (vector-ref m row) col)))
  
  (define mat-rows
    (lambda (m)
      (vector-length m)))
  
  (define mat-cols
    (lambda (m)
      (vector-length (vector-ref m 0))))
  
  (define mat-shape
    (lambda (m)
      (cons (mat-rows m) (mat-cols m))))
  
  (define mat?
    (lambda (m)
      (and (vector? m) (vector? (vector-ref m 0)))))
  
  (define mat??
    (lambda (m)
      (and (vector? m)
           (let ([cols (mat-cols m)]
                 [rows (mat-rows m)])
             (let sub-mat?? ([i 1])
               (if (= i rows)
                   #t
                   (let ([row (vector-ref m i)])
                     (and (vector? row)
                          (= cols (vector-length row))
                          (sub-mat?? (+ i 1))))))))))
  
  (define mat-fill!
    (case-lambda
      [(m val)
       (vector-for-each
        (lambda (v) (vector-fill! v val))
        m)]
      [(m top left height width val)
       (let ([bottom (+ top height)]
             [right (+ left width)])
         (do ([i top (+ i 1)])
             ((= i bottom) m)
           (do ([j left (+ j 1)])
               ((= j right))
             (mat-set! m i j val))))]))
  
  (define mat-slice
    (lambda (m top left height width)
      (let ([mat (make-mat height width)])
        (do ([i 0 (+ i 1)]
             [i2 top (+ i2 1)])
            ((= i height) mat)
          (do ([j 0 (+ j 1)]
               [j2 left (+ j2 1)])
              ((= j width))
            (mat-set! mat i j
                         (mat-ref m i2 j2)))))))
  
  (define mat-row
    (lambda (m i)
      (vector-ref m i)))
  
  (define mat-col
    (lambda (m j)
      (vector-map
       (lambda (row-v)
         (vector-ref row-v j)) m)))
  
  (define mat-copy
    (lambda (m)
      (vector-map
       vector-copy
       m)))
  
  (define mat-paste!
    (case-lambda
      [(dst src top left)
       (mat-paste! dst src
                      top left 0 0 (mat-rows src) (mat-cols src))]
      [(dst src tdst ldst tsrc lsrc height width)
       (let ([height (min height (- (mat-rows src) tsrc) (- (mat-rows dst) tdst))]
             [width (min width (- (mat-cols src) lsrc) (- (mat-cols dst) ldst))])
         (let ([bsrc (+ tsrc height)]
               [rsrc (+ lsrc width)])
           (do ([i tsrc (+ i 1)]
                [i2 tdst (+ i2 1)])
               ((= i bsrc) dst)
             (do ([j lsrc (+ j 1)]
                  [j2 ldst (+ j2 1)])
                 ((= j rsrc))
               (mat-set! dst i2 j2 (mat-ref src i j))))))]))
  
  (define mat-paste
    (case-lambda
      [(dst src top left)
       (mat-paste dst src
                     top left 0 0 (mat-rows src) (mat-cols src))]
      [(dst src tdst ldst tsrc lsrc height width)
       (let ([height (min height (- (mat-rows src) tsrc) (- (mat-rows dst) tdst))]
             [width (min width (- (mat-cols src) lsrc) (- (mat-cols dst) ldst))]
             [dst (mat-copy dst)])
         (let ([bsrc (+ tsrc height)]
               [rsrc (+ lsrc width)])
           (do ([i tsrc (+ i 1)]
                [i2 tdst (+ i2 1)])
               ((= i bsrc) dst)
             (do ([j lsrc (+ j 1)]
                  [j2 ldst (+ j2 1)])
                 ((= j rsrc))
               (mat-set! dst i2 j2 (mat-ref src i j))))))]))
  
  (define mat-print
    (lambda (m)
      (vector-for-each
       (lambda (v)
         (display v)
         (newline)) m)))
  
  (define list->mat
    (lambda (ml)
      (list->vector (map list->vector ml))))

    (define vector-cat
    (lambda vs
      (let ([v (make-vector
                (fold-left
                 (lambda (a x)
                   (+ a (vector-length x))) 0
                   vs))]
            [i 0])
        (for-each
         (lambda (x)
           (vector-for-each
            (lambda (va)
              (vector-set! v i va)
              (set! i (+ i 1))) x)) vs) v)))
  
  (define vector-rotate
    (case-lambda
      [(l step)
       (let ([len (vector-length l)]
             [result (vector-copy l)])
         (do ([i 0 (+ i 1)])
             ((= i len) result)
           (vector-set! result i (vector-ref l (mod (+ i step) len)))))]
      [(l) (vector-rotate 1)]))
  
  (define list-rotate
    (lambda (l)
      (append (cdr l) (list (car l)))))
  
  (define vector-mul
    (lambda (v1 v2)
      (vector-map
       (lambda (val1 val2) (* val1 val2))
       v1 v2)))
  
  (define vector-dot
    (lambda (v1 v2)
      (set-virtual-register! 0 0)
      (vector-for-each
         (lambda (val1 val2) (set-virtual-register! 0 (+ (virtual-register 0) (* val1 val2))))
         v1 v2)
        (virtual-register 0)))
  
  (define vector-cross
    (lambda (v1 v2)
      (let ([a (vector-ref v1 0)] [b (vector-ref v1 1)] [c (vector-ref v1 2)]
            [d (vector-ref v2 0)] [e (vector-ref v2 1)] [f (vector-ref v2 2)])
        (vector (- (* b f) (* c e))
                (- (* c d) (* a f))
                (- (* a e) (* b d))))))
  
  (define vector-reform
    (lambda (v . inx)
      (list->vector
       (let ([inx (if (list? (car inx))
                      (car inx)
                      inx)])
         (map
          (lambda (ind)
            (if (symbol? ind)
                (vector-axis v ind)
                (vector-ref v ind)))
          inx)))))
  
  (define vector-axis
    (lambda (v ax)
      (case ax
        ['x (vector-ref v 0)]
        ['y (vector-ref v 1)]
        ['z (vector-ref v 2)]
        ['w (vector-ref v 3)])))
  
  (define vector-norm
    (lambda (v)
      (sqrt (vector-dot v v))))
  
  (define vector->mat
    (case-lambda
      [(v) (vector v)]
      [(v rows cols)
       (let ([len (vector-length v)]
             [mat (make-mat rows cols 0)])
         (do ([i 0 (+ i 1)]
              [base 0 (+ base cols)])
             ((= i rows) mat)
           (do ([j 0 (+ j 1)]
                [ind base (+ ind 1)])
               ((or (= j cols) (>= ind len)))
             (mat-set! mat i j (vector-ref v ind)))))]
      [(v cols)
       (vector->mat v
                       (ceiling (/ (vector-length v) cols))
                       cols)]))
  
  (define vector-outer
    (lambda (u v)
      (let ([ul (vector-length u)]
            [vl (vector-length v)])
        (let ([mat (make-mat ul vl)])
          (do ([i 0 (+ i 1)])
              ((= i ul) mat)
            (let ([vec (vector-ref mat i)]
                  [uval (vector-ref u i)])
              (do ([j 0 (+ j 1)])
                  ((= j vl))
                (vector-set! vec j (* uval (vector-ref v j))))))))))
  
  (define mat->vector
    (lambda (m)
      (apply vector-cat (vector->list m))))
  
  (define-syntax vector-ops
    (syntax-rules ()
      [(_ v) (vector-map - v)]))
  
  (define-syntax vector-add
    (syntax-rules ()
      [(_ u v) (vector-map + u v)]
      [(_ u v ...) (vector-add (vector-add u v) ...)]))
  
  (define-syntax vector-scale
    (syntax-rules ()
      [(_ u k) (vector-map
                (lambda (x) (* k x)) u)]))
     (define mat-reform
       (case-lambda
         [(mat cols)
          (vector->mat (mat->vector mat) cols)]
         [(mat rows cols)
          (vector->mat (mat->vector mat) rows cols)]))
     
     (define-syntax mat-scale
       (syntax-rules ()
         [(_ mat k)
          (vector-map (lambda (v)
                        (vector-scale v k))
                      mat)]))
     
     (define-syntax mat-add
       (syntax-rules ()
         [(_ m n) (vector-map (lambda (u v) (vector-add u v))
                              m n)]
         [(_ m n ...) (mat-add (mat-add m n) ...)]))
     
     (define-syntax mat-ops
       (syntax-rules ()
         [(_ m) (vector-map vector-ops m)]))
     
     (define mat-mmul
       (lambda (m n)
         (let ([i-max (mat-rows m)]
               [k-max (mat-cols m)]
               [j-max (mat-cols n)])
           (let ([mat (make-mat i-max j-max)])
             (do ([k 0 (+ k 1)])
                 ((= k k-max) mat)
               (do ([i 0 (+ i 1)])
                   ((= i i-max))
                 (let ([r (mat-ref m i k)])
                   (do ([j 0 (+ j 1)])
                       ((= j j-max))
                     (mat-set! mat i j
                                  (+ (mat-ref mat i j)
                                     (* (mat-ref n k j) r)))))))))))
     
     (define mat-mul
       (lambda (m n)
         (vector-map
          (lambda (u v)
            (vector-mul u v))
          m n)))
     
     (define mat-dot
       (lambda (m n)
         (set-virtual-register! 0 0)
         (vector-for-each
          (lambda (v1 v2)
            (set-virtual-register! 0
                                   (+ (virtual-register 0) (vector-dot v1 v2))))
          m n)
         (virtual-register 0)))
     
     (define mat-trans
       (lambda (m)
         (let ([rows (mat-rows m)]
               [cols (mat-cols m)])
           (let ([mat (make-mat rows cols)])
             (do ([i 0 (+ i 1)])
                 ((= i rows) mat)
               (do ([j 0 (+ j 1)])
                   ((= j cols))
                 (mat-set! mat i j
                              (mat-ref m j i))))))))

       (define pi
    (* 2 (asin 1.0)))
  
  (define deg->rad
    (lambda (deg)
      (* (/ deg 180) pi)))
  
  (define rad->deg
    (lambda (rad)
      (/ (* rad 180) pi)))
  
  ;; Quaternion structure
  
  (define-structure (quaternion vec theta))
  
  (define quaternion-mul
    (lambda (q1 q2)
      (let ([v1 (quaternion-vec q1)]
            [w1 (quaternion-theta q1)]
            [v2 (quaternion-vec q2)]
            [w2 (quaternion-theta q2)])
        (make-quaternion
         (- (* w1 w2)
            (vector-dot v1 v2))
         (vector-add (vector-cross v1 v2)
                     (vector-scale v1 w2)
                     (vector-scale v2 w1))))))
  
  (define quaternion-conjugate
    (lambda (q)
      (make-quaternion
       (quaternion-theta q)
       (vector-ops (quaternion-vec q)))))
  
  (define quaternion-norm2
    (lambda (q)
      (let ([v (quaternion-vec q)]
            [w (quaternion-theta q)])
        (+ (vector-dot v v)
           (* w w)))))
  
  (define quaternion-inv
    (lambda (q)
      (let ([norm2 (quaternion-norm2 q)])
        (make-quaternion
         (/ (quaternion-theta q) norm2)
         (vector-scale (quaternion-vec q) (/ norm2))))))
  
  (define make-vecq
    (case-lambda
      [(x y z w) (vector x y z w)]
      [(x y z) (vector x y z
                       (sqrt (+ (* x x) (* y y) (* z z))))]))
  
  (define vecq-mul
    (lambda (p q)
      (let ([px (vector-ref p 0)]
            [py (vector-ref p 1)]
            [pz (vector-ref p 2)]
            [pw (vector-ref p 3)]
            [qx (vector-ref q 0)]
            [qy (vector-ref q 1)]
            [qz (vector-ref q 2)]
            [qw (vector-ref q 3)])
        (vector
         (- (* pw qw) (* px qx) (* py qy) (* pz qz))
         (- (+ (* px qw) (* pw qx) (* py qz)) (* pz qy))
         (- (+ (* py qw) (* pw qy) (* pz qx)) (* px qz))
         (- (+ (* pz qw) (* pw qz) (* px qy)) (* py qx))))))
  
  (define vecq-conj
    (lambda (q)
      (vector
       (- (vector-ref q 0))
       (- (vector-ref q 1))
       (- (vector-ref q 2))
       (vector-ref q 3))))
  
  (define vecq-norm2
    (lambda (q)
      (vector-dot q q)))
  
  (define vecq-inv
    (lambda (q)
      (vector-scale (vecq-conj q) (/ (vecq-norm2 q)))))
  
  (define vecq-exp
    (lambda (q)
      (let* ([v (vector-reform q 0 1 2)]
             [theta (vector-norm v)])
        (vector-scale (vector-cat (vector-scale v (/ (sin theta) theta))
                                  (vector (cos theta)))
                      (exp (vector-ref q 3))))))
  
  (define vecq-log
    (lambda (q)
      (let* ([qnorm (vector-norm q)]
             [unitq (vector-scale q (/ qnorm))]
             [theta (acos (vector-ref unitq 3))]
             [result (vector-scale unitq (/ theta (sin theta)))])
        (vector-set! result 3 (log qnorm))
        result)))
  
  (define vecq-rot
    (lambda (v theta)
      (vector-cat (vector-scale v (/ (sin (/ theta 2)) (vector-norm v)))
                  (vector (cos (/ theta 2)))))))