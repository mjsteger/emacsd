(load-file "~/.emacs.d/tweaks/rainbow-delimiters.el")

(define-global-minor-mode global-rainbow-mode rainbow-delimiters-mode
 (lambda () (rainbow-delimiters-mode 1)))


 ;; Turn on global rainbow-delimiters mode, which colors (),{},[] to
 ;; show matching/deepness
(global-rainbow-mode)

