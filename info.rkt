#lang info
(define collection "minecraft-ping")
(define deps '("base" "typed-racket-lib" "json-type-provider" "bitsyntax"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "rackunit-typed"))
(define scribblings '())
(define pkg-desc "Ping Minecraft servers for status information")
(define version "0.0")
(define pkg-authors '(winny))