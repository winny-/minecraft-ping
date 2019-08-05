#lang typed/racket/base

(require racket/list
         racket/match

         bitsyntax

         "varint.rkt")

(struct untyped-packet ([id : Integer]
                        [payload : Bytes])
  #:transparent)

(define-syntax packet
  (syntax-rules ()
    [(_ #t input ks kf)
     (bit-string-case input
       ([(len :: (var-int))
         (packet-id :: (var-int))
         (payload :: binary bytes len)
         (rest :: binary)]
        (ks (cast (untyped-packet packet-id (bit-string->bytes payload)) untyped-packet ) rest)))]
    [(_ #f pkt)
     (match-let ([(struct untyped-packet (id payload)) pkt])
       (bit-string
         [(bytes-length payload)  :: (var-int)]
         [id :: (var-int)]
         [payload :: binary]))]))

(: f (Bytes -> untyped-packet))
(define (f b)
  (bit-string-case (cast b BitString)
    ([( pkt :: (packet))] pkt)))

(module+ test
  (require typed/rackunit)
  (define-syntax-rule (check-packet-encode input expected)
    (check-equal? (bit-string->bytes (bit-string (input :: (packet))))
                  expected
                  (format "in: ~v out: ~v" input expected)))
  (define-syntax-rule (check-packet-decode input expected)
    (check-equal? (bit-string-case (cast input BitString)
                                   ([(res :: (packet))] res)) expected
                  (format "in: ~v out: ~v" input expected)))

  (define testcases
    `((,(untyped-packet 0 #"") #"\x0\x0")
      (,(untyped-packet 1 #"") #"\x0\x1")))
  (test-case "packet-encode"
    (for ([t testcases])
      (check-packet-encode (first t) (second t))))
  (test-case "packet-decode"
    (for ([t testcases])
      (check-packet-decode (second t) (first t)))))
