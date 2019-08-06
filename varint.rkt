#|
https://wiki.vg/Protocol#VarInt_and_VarLong

Thanks to @jaz in the Racket Slack for supplying working implementations of main logic.
https://gist.github.com/97jaz/2884d660bb16d006a2c099639ec4b893
|#

#lang typed/racket/base

(require racket/function
         racket/list
         racket/match
         bitsyntax)

(provide var-int)

(: signed-var-int (Integer -> Integer))
(define (signed-var-int val)
  (if (bitwise-bit-set? val 31)
      (bitwise-ior val (bitwise-not #xffffffff))
      val))

(: integer->var-int (Integer -> Bytes))
(define (integer->var-int int)
  (when (> (integer-length int) 31)
    (raise-argument-error 'var-int "integer between -2^31 and (2^31)-1" 0 int))
  (let loop ([ls : (Listof Integer) empty]
             [rem (bitwise-and int #xffffffff)])
    (define b (bitwise-and rem #b01111111))
    (match (arithmetic-shift rem -7)
      [0 (list->bytes (reverse (cons b ls)))]
      [rem-next (loop (cons (bitwise-ior b #b10000000) ls)
                      rem-next)])))

(define-syntax var-int
  (syntax-rules ()
    [(_ #t input ks kf)
     (let loop ([pending input] [num-read 0] [result 0])
       (bit-string-case pending
         ([(b :: unsigned integer bytes 1) (rest :: binary)]
          (define value (bitwise-and b #b01111111))
          (define new-result
            (bitwise-ior result
                         (arithmetic-shift value (* 7 num-read))))
          (define new-num-read (add1 num-read))
          (cond
            ;; Bad VarInt
            [(> new-num-read 5) (kf)]
            [(zero? (bitwise-and b #b10000000)) (ks (signed-var-int new-result) rest)]
            [else (loop rest new-num-read new-result)]))
         ;; Short
         (else
          (kf #t))))]
    [(_ #f int)
     ;; This is extracted into a procedure to give type annotations
     (integer->var-int int)]))

(module+ test
  (require typed/rackunit)

  (define testcases
    '((0 #"\000")
      (1 #"\001")
      (2 #"\002")
      (1024 #"\x80\x08")
      (25565 #"\xdd\xc7\x01")
      (127 #"\177")
      (128 #"\200\1")
      (255 #"\377\1")
      (2147483647 #"\377\377\377\377\a")
      (-1 #"\377\377\377\377\17")
      (-2147483648 #"\200\200\200\200\b")))
  (define-syntax-rule (check-var-int-encode input expected)
    (check-equal? (bit-string (input :: (var-int))) expected
                  (format "in: ~v out: ~v" input expected)))
  (define-syntax-rule (check-var-int-decode input expected)
    (check-equal? (bit-string-case (cast input BitString)
                    ([(res :: (var-int))] res)
                    (else #f))
                  expected
                  (format "in: ~v out: ~v" input expected)))

  (test-case "var-int-encode"
    (for ([t testcases])
      (check-var-int-encode (first t) (second t)))
    (check-exn exn:fail:contract? (thunk (bit-string [(expt 2 31) :: (var-int)])))
    (check-exn exn:fail:contract? (thunk (bit-string [(sub1 (expt -2 31)) :: (var-int)]))))
  (test-case "var-int-decode"
    (for ([t testcases])
      (check-var-int-decode (second t) (first t)))
    (check-var-int-decode #"\377\377\377\377\377" #f)
    (check-var-int-decode #"\377\377\377\377\377\17" #f)
    (check-var-int-decode #"\377\377\377\377\377\377\17" #f)))
