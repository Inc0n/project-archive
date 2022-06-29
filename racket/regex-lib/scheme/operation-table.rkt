
#lang racket

(require "2D-table.rkt")

(provide get put)

(define operation-table (make-table))
(define get (operation-table 'lookup))
(define put (operation-table 'insert!))