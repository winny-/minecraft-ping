#lang typed/racket/base

(require racket/function
         racket/list
         racket/match
         bitsyntax)

(provide var-int)

(: logical-right-shift (Integer Integer . -> . Integer))
(define (logical-right-shift x n)
  (integer-length x)
  (arithmetic-shift x (- n)))

(define N : Integer (integer-bytes->integer #"\1\377\377\377\377\377\377\377" #t #t))

(define-syntax var-int
  (syntax-rules ()
    [(_ #t input ks kf)
     (let loop ([result 0]
                [num-read 0]
                [work input])
       (when (>= num-read 5)
         (raise-user-error 'var-int "too large"))
       (bit-string-case work
         ([(b :: integer bytes 1) (rest :: binary)]
          (define value (bitwise-and b #b1111111))
          (define result2 (bitwise-ior result
                                       (arithmetic-shift value (* 7 num-read))))
          (if (zero? (bitwise-and b #b10000000))
              (ks result2 rest)
              (loop result2 (add1 num-read) rest)))))]
    [(_ #f int)
     (let loop ([ls : (Listof Integer) empty]
                [rem int])
       ;;       (printf "~a: ~a ~a\n" int rem ls)
       (define b (bitwise-and rem #b01111111))
       (match (arithmetic-shift rem -7)
         [0 (list->bytes (reverse (cons b ls)))]
         [rem-next (loop (cons (bitwise-ior b #b10000000) ls)
                         rem-next)]))]))

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
      #;      (-1 #"\377\377\377\377\17")
      #;      (-2147483648 #"\200\200\200\200\b")
      ))
  (define-syntax-rule (check-var-int-encode input expected)
    (check-equal? (bit-string (input :: (var-int))) expected
                  (format "in: ~v out: ~v" input expected)))
  (define-syntax-rule (check-var-int-decode input expected)
    (check-equal? (bit-string-case (cast input BitString)  ([(res :: (var-int))] res)) expected
                  (format "in: ~v out: ~v" input expected)))

  (test-case "var-int-encode"
    (for ([t testcases])
      (check-var-int-encode (first t) (second t))))
  (test-case "var-int-decode"
    (for ([t testcases])
      (check-var-int-decode (second t) (first t)))))
