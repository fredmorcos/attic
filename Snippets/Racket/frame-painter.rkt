#lang racket

(require racket/class
         racket/gui/base
         slideshow
         slideshow/code)

(define main-frame (new frame% 
                        [label "Main Frame"]
                        [width 300]
                        [height 300]
                        [min-width 300]
                        [min-height 300]
                        [stretchable-width #t]
                        [stretchable-height #t]
                        [alignment '(center center)]))
(send main-frame show #t)

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas%
         [parent main-frame]
         [style '(border)]
         [paint-callback (lambda (self dc) (drawer dc 0 0))])))

(add-drawing (code (circle 10)))
(add-drawing (code ("hello world!")))