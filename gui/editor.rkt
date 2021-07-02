#lang racket/gui

(provide editor-main)


(define (editor-main)
  (define f (new frame%
                 [label "Automata Theory Tutor"]
                 [width 300]
                 [height 300]))
  (define c (new editor-canvas% [parent f]))
  (define t (new text%))
  (define mb (new menu-bar% [parent f]))
  (define m-file (new menu% [label "File"] [parent mb]))
  (define m-edit (new menu% [label "Edit"] [parent mb]))
  (define mi-open
    (new menu-item%
         [label "Open"]
         [parent m-file]
         [callback
          (λ (i e)
            (define path (get-file #f f))
            (when path
              (send t load-file path 'text)))]
         [shortcut #\o]
         [shortcut-prefix '(ctl)]))
  (define mi-save
    (new menu-item%
         [label "Save"]
         [parent m-file]
         [callback
          (λ (i e)
            (send t save-file #f 'text))]
         [shortcut #\s]
         [shortcut-prefix '(ctl)]))
  (append-editor-operation-menu-items m-edit #f)
  (send t set-max-undo-history 100)  
  (send c set-editor t)
  (send f show #t))
                 
