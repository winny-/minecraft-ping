#lang typed/racket/base

(require json-comb
         racket/list)

(provide (all-defined-out))


(define-json-types
  [Response ([version : Version]
             [players : Players]
             [description : (U String Description)]
             [favicon : String #:default [#f : #f]])]
  [Version ([name : String]
            [protocol : JSNum])]
  [Players ([max : JSNum]
            [online : JSNum]
            [sample : (Listof Sample) #:default [empty : (Listof Sample)]])]
  [Sample ([name : String]
           [id : String])]
  [Description (([text : String]) => String #:by text)])


(module+ test
  (require typed/rackunit)
  (test-case "json"
    (check-equal? (call-with-input-file "t1.json" read-Response)
                  (Response (Version "1.8.7" 47)
                            (Players 100 5
                                     (list (Sample "thinkofdeath"
                                                   "4566e69f-c907-48ee-8d71-d7ba5aa00d20")))
                            "Hello world"
                            "data:image/png;base64,<data>"))
    (check-equal? (call-with-input-file "t2.json" read-Response)
                  (Response (Version "1.8.7" 47)
                            (Players 100 5
                                     (list))
                            "Hello world"
                            "data:image/png;base64,<data>"))
    (check-equal? (call-with-input-file "t3.json" read-Response)
                  (Response (Version "1.8.7" 47)
                            (Players 100 5
                                     (list (Sample "thinkofdeath"
                                                   "4566e69f-c907-48ee-8d71-d7ba5aa00d20")))
                            "Hello world"
                            #f))
    (check-equal? (call-with-input-file "t4.json" read-Response)
                  (Response (Version "1.8.7" 47)
                            (Players 100 5
                                     (list (Sample "thinkofdeath"
                                                   "4566e69f-c907-48ee-8d71-d7ba5aa00d20")))
                            "Basic description"
                            "data:image/png;base64,<data>"))))
