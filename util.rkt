#lang typed/racket/base

(require racket/match)

(provide string->host+port)

(: string->host+port ((String) ((Option Exact-Nonnegative-Integer)) . ->* . (Option (List String (Option Exact-Nonnegative-Integer)))))
(define (string->host+port s [default-port #f])
  (: string->port (String -> (Option Exact-Nonnegative-Integer)))
  (define (string->port s)
    (define n (string->number s))
    (if (exact-nonnegative-integer? n)
        n
        default-port))
  (match s
    [(regexp #px"^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$"
             (list h))
     (list h default-port)]
    [(regexp #px"^([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}):([0-9]+)$"
             (list _ h p))
     (list (cast h String) (string->port (cast p String)))]
    [(regexp #px"^(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$"
             (list h _ ...))
     (list h default-port)]
    [(regexp #px"^\\[(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))\\]:([0-9]+)$"
             (list _ h _ ... p))
     (list (cast h String) (string->port (cast p String)))]
    [(regexp #px"^([A-Za-z0-9_-]+\\.?)+$" (list h _ ...))
     (list h default-port)]
    [(regexp #px"^(([A-Za-z0-9_-]+\\.?)+):([0-9]+)$" (list _ h _ ... p))
     (list (cast h String) (string->port (cast p String)))]
    [_ #f]))

(module+ test
  (require typed/rackunit)
  (test-case "string->host+port"
    (check-equal? (string->host+port "127.0.0.1") '("127.0.0.1" #f))
    (check-equal? (string->host+port "1.1.1.1:53") '("1.1.1.1" 53))
    (check-equal? (string->host+port "::1") '("::1" #f))
    (check-equal? (string->host+port "[::1]:523") '("::1" 523))
    (check-equal? (string->host+port "www.google.com") '("www.google.com" #f))
    (check-equal? (string->host+port "www.google.com.:80") '("www.google.com." 80))))
