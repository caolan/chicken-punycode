(module punycode *

(import chicken scheme)
(use utf8 utf8-srfi-13 ports miscmacros)

;; Bootstring parameters for Punycode
(define base 36)
(define tmin 1)
(define tmax 26)
(define skew 38)
(define damp 700)
(define initial-bias 72)
(define initial-code #x80) ;; first non-ascii code point
(define delimiter #\-)

(define (ascii? c)
  (char<=? c #\x7f))

(define (string-insert-at str i new)
  (string-append (string-take str i) new (string-drop str i)))

(define digits
  (string-append
    "abcdefghijklmnopqrstuvwxyz"    ;; 0 to 25, respectively
    "0123456789"))                  ;; 26 to 35, respectively

(define (encode-digit d #!optional uppercase)
  (let ((c (string-ref digits d)))
    (if uppercase (char-upcase c) c)))

(define (decode-digit d)
  (string-index digits (char-downcase d)))

;; 6.1 Bias adaptation function
(define (adapt delta num first)
  (let ((d0 (fx/ delta (if first damp 2))))
    (let loop ((d (fx+ d0 (fx/ d0 num)))
               (k 0))
      (if (fx> d (fx/ (fx* (fx- base tmin) tmax) 2))
        (loop (fx/ d (fx- base tmin)) (fx+ k base))
        (fx+ k (fx/ (fx* (fx+ (fx- base tmin) 1) d) (fx+ d skew)))))))

(define (encode-integer n bias #!optional uppercase)
  (with-output-to-string
    (lambda ()
      (let loop ((q n)
                 (k base))
        (let ((t (cond ((<= k bias) tmin)
                       ((>= k (fx+ bias tmax)) tmax)
                       (else (- k bias)))))
          (if (< q t)
            (display (encode-digit q uppercase))
            (begin
              (display (encode-digit (+ t (modulo (- q t) (- base t))) #f))
              (loop (fx/ (- q t) (- base t))
                    (+ k base)))))))))

(define (decode-integer str bias #!optional (start 0))
  (let loop ((n 0)
             (i start)
             (w 1)
             (k base))
    (let ((digit (decode-digit (string-ref str i)))
          (t (cond ((<= k bias) tmin)
                   ((>= k (+ bias tmax)) tmax)
                   (else (- k bias)))))
      (when (> digit base) (abort "punycode bad input"))
      (if (>= digit t)
        (loop (+ n (* digit w))
              (+ i 1)
              (* w (- base t))
              (+ k base))
        (values (+ n (* digit w))
                (+ i 1)
                (char-upper-case? (string-ref str i)))))))

(define (decode-integers str out #!optional (bias initial-bias) (start 0))
  (if (< start (string-length str))
    (receive (n i upper) (decode-integer str bias start)
      (if (< i (string-length str))
        (cons (cons n upper)
              (decode-integers str
                               (+ out 1)
                               (adapt n (+ out 1) (= start 0))
                               i))
        (list (cons n upper))))
    '()))

;; finds next-lowest code point beginning with 'start' (inclusive),
;; or returns #f if there are no code points higher or equal to 'start'
(define (next-code-point start str)
  (string-fold
    (lambda (c m)
      (let ((n (char->integer c)))
        (if (>= n start)
          (if m
            (if (< n m) n m)
            n)
          m)))
    #f
    str))

(define (encode-non-ascii basic-length str)
  (with-output-to-string
    (lambda ()
      (let ((total (string-length str))
            (handled basic-length)
            (start initial-code)
            (delta 0)
            (bias initial-bias))
        (while (< handled total)
          (and-let* ((code (next-code-point start str))
                     (i 0))
            (set! delta (+ delta (* (- code start) (+ handled 1))))
            (string-for-each
              (lambda (c)
                (inc! i)
                (cond
                  ((< (char->integer c) code) (inc! delta))
                  ((= (char->integer c) code)
                   (display (encode-integer delta bias (char-upper-case? c)))
                   (set! bias
                     (adapt delta (+ handled 1) (= handled basic-length)))
                   (set! delta 0)
                   (inc! handled))))
                str)
            (set! start (+ code 1))
            (inc! delta)))))))

(define (punycode-encode str)
  (let ((ascii-chars (string-filter ascii? str)))
    (string-append/shared
      ascii-chars
      (if (> (string-length ascii-chars) 0) (list->string (list delimiter)) "")
      (encode-non-ascii (string-length ascii-chars) str))))

(define (decode-non-ascii basic encoded)
  (let loop ((result basic)
             (code initial-code)
             (out (string-length basic))
             (i 0)
             (encoded encoded))
    (if (null? encoded)
      result
      (let* ((enc (car encoded))
             (delta (car enc))
             (upper (cdr enc))
             (code (+ code (fx/ (+ i delta) (+ out 1))))
             (i (modulo (+ i delta) (+ out 1))))
        (loop (string-insert-at result i (code->string code upper))
              code
              (+ out 1)
              (+ i 1)
              (cdr encoded))))))

(define (code->string n #!optional uppercase)
  (let ((ch (integer->char n)))
    (list->string
      (list (if uppercase (char-upcase ch) ch)))))

(define (punycode-decode str)
  (let* ((delim (string-index-right str delimiter))
         (basic (if delim (string-take str delim) str))
         (deltas (and delim (string-drop str (+ delim 1)))))
    (if deltas
      (decode-non-ascii basic (decode-integers deltas (string-length basic)))
      ;; whole string non-ascii
      (decode-non-ascii "" (decode-integers basic 0)))))

)
