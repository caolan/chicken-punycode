(module punycode

;; exports
(punycode-encode
 punycode-decode
 domain->ascii
 domain->unicode)

(import chicken scheme)

(use ports
     utf8
     utf8-srfi-13
     utf8-srfi-14
     data-structures
     miscmacros)

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

(define encode-digit
  (cut string-ref digits <>))

(define (decode-digit d)
  (string-index digits (char-downcase d)))

;; 6.1 Bias adaptation function
(define (adapt delta num first)
  (let ((d0 (quotient delta (if first damp 2))))
    (let loop ((d (+ d0 (quotient d0 num)))
               (k 0))
      (if (> d (quotient (* (- base tmin) tmax) 2))
        (loop (quotient d (- base tmin)) (+ k base))
        (+ k (quotient (* (+ (- base tmin) 1) d) (+ d skew)))))))

(define (encode-integer n bias)
  (with-output-to-string
    (lambda ()
      (let loop ((q n)
                 (k base))
        (let ((t (cond ((<= k bias) tmin)
                       ((>= k (+ bias tmax)) tmax)
                       (else (- k bias)))))
          (if (< q t)
            (display (encode-digit q))
            (begin
              (display (encode-digit (+ t (modulo (- q t) (- base t)))))
              (loop (quotient (- q t) (- base t))
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

(define (calculate-delta prev code handled #!optional (delta 0))
  (+ delta (* (- code prev) (+ handled 1))))

(define (encode-code-point str len handled bias code delta first)
  (let loop ((handled handled)
             (bias bias)
             (delta delta)
             (index 0))
    (if (< index len)
      (let ((n (char->integer (string-ref str index))))
        (cond ((< n code)
               (loop handled
                     bias
                     (+ delta 1)
                     (+ index 1)))
              ((= n code)
               (display (encode-integer delta bias))
               (loop (+ handled 1)
                     (adapt delta (+ handled 1) first)
                     0
                     (+ index 1)))
              (else
                (loop handled
                      bias
                      delta
                      (+ index 1)))))
      (values handled bias delta))))

(define (encode-non-ascii basic-length str)
  (with-output-to-string
    (lambda ()
      (let ((len (string-length str)))
        (let loop ((handled basic-length)
                   (bias initial-bias)
                   (code initial-code)
                   (delta 0))
          (when (< handled len)
            (let* ((c (next-code-point code str))
                   (d (calculate-delta code c handled delta))
                   (first (= handled basic-length)))
              (receive (handled bias d)
                (encode-code-point str len handled bias c d first)
                (loop handled
                      bias
                      (+ c 1)
                      (+ d 1))))))))))

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
             (code (+ code (quotient (+ i delta) (+ out 1))))
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


;; RFC 3490
(define label-separators
  (string->char-set "\x2E\u3002\uFF0E\uFF61"))

(define (map-domain proc str)
  (string-join
    (map proc (string-tokenize str (char-set-complement label-separators)))
    "."))

(define (punycode-domain? str)
  (string-prefix? "xn--" str))

(define (unicode-domain? str)
  (string-any (complement ascii?) str))

;; Converts a domain possibly containing unicode characters to a punycode
;; representation of the domain, ascii-only domains are untouched. This
;; procedure is idempotent.
(define (domain->ascii str)
  (map-domain
    (lambda (s)
      (if (unicode-domain? s)
        (string-append "xn--" (punycode-encode s))
        s))
    str))

;; Converts a possibly punycoded domain back to it's unicode
;; representation. Unicode strings are returned unmodified. This procedure
;; is idempotent.
(define (domain->unicode str)
  (map-domain
    (lambda (s)
      (if (punycode-domain? s)
        (punycode-decode (string-drop s 4))
        s))
    str))

)
