#lang typed/racket/base

(provide mc-string)

(require bitsyntax

         "varint.rkt")

(: bit-string->mc-string (BitString -> String))
(define (bit-string->mc-string bs)
  (bit-string-case bs
    ([(len :: (var-int)) (bs1 :: binary bytes len)]
     (bytes->string/utf-8 (bit-string->bytes bs1)))))


(define-syntax mc-string
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
                      ;; causes some weird error
                      ;; #:on-short (λ (x : Any) (kf #t))
                      ([(len :: (var-int)) (payload :: binary bytes len) (rest :: binary)]
                       (ks (bytes->string/utf-8 (bit-string->bytes payload)) rest))
                      (else (kf #f)))]
    [(_ #f str)
     (let ([bs (string->bytes/utf-8 str)])
       (bit-string [(bytes-length bs) :: (var-int)]
                   [bs :: binary]))]))

(module+ test
  (require typed/rackunit)
  (check-equal? (bit-string-case (cast #"\b\303\247\303\260\303\266\302\243" BitString)
                                 ([(str :: (mc-string))]
                                  str))
                "çðö£")
  (check-equal? (bit-string->bytes (bit-string ("çðö£" :: (mc-string))))
                #"\b\303\247\303\260\303\266\302\243"))