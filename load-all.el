(load-file "~/.emacs.d/org-mode/load-org.el")
(load-file "~/.emacs.d/languages/load-languages.el")
(load-file "~/.emacs.d/tweaks/load-tweaks.el")
(load-file "~/.emacs.d/vendors/load-vendors.el")
(load-file "~/.emacs.d/flymake/load-flymake.el")
(load-file "~/.emacs.d/vcs/load-vcs.el")

;; (add-to-list 'load-path "~/.emacs.d/predictive/")
;; (set-default 'predictive-auto-add-to-dict t)
;; (require 'predictive)
(add-to-list 'load-path "~/.emacs.d/auctex-11.86/")
(add-to-list 'load-path "~/.emacs.d/auctex-11.86/preview/")
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(load-file "~/.emacs.d/goto-last-change.el")
(global-set-key (kbd "C-c f") 'goto-last-change)


(add-to-list 'load-path "~/.emacs.d/elisp/external")
(require 'gist)
(setq gist-authenticate-function 'gist-basic-authentication)

(add-to-list 'load-path "~/.emacs.d/perspective")
(require 'perspective)
(persp-mode)

(global-set-key (kbd "C-c p a") 'persp-add-buffer)
(global-set-key (kbd "C-c p b") 'persp-switch)
(global-set-key (kbd "C-c p d") 'persp-remove-buffer)
(persp-turn-on-modestring)
