#lang scheme 
(require (planet schematics/schemeunit:3))

; (car ''abracadabra)
; after sustituting (quote x) for 'x:
; (car (quote (quote abracadabra)))
; evaluating car, we get the first element in (quote (quote abracadabra)),
; which is quote.

(check-equal? (car ''abracadabra) 'quote)
(check-equal? (car (quote (quote abracadabra))) 'quote)