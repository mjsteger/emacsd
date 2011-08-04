;; this is enabled if you use emacs with the fullscreen slipped in
;; the easiest way to get this is brew install emacs --cocoa
(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                         'fullboth)))
(global-set-key (kbd "<C-return>") 'ns-toggle-fullscreen)

;Allows syntax highlighting to work, among other things

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path "~/.emacs.d/tweaks/auto-complete-1.3.1")
(add-to-list 'load-path "~/.emacs.d/tweaks/")
(require 'auto-complete-config)
(ac-config-default)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.05)
 '(ac-quick-help-delay 2.0)
 '(ac-show-menu-immediately-on-auto-complete nil)
 '(ecb-options-version "2.40")
 '(gud-gdb-command-name "gdb --annotate=1")
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(large-file-warning-threshold nil)
 '(ropemacs-confirm-saving nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; gives Shell-pop, which lets you pop up a shell. More info here:
;; http://www.emacswiki.org/emacs/ShellPop
(require 'shell-pop)
;; (shell-pop-set-internal-mode "ansi-term")
;; (shell-pop-set-internal-mode-shell "/bin/zsh")
;; (shell-pop-set-internal-mode "shell")
(shell-pop-set-internal-mode "eshell")
(shell-pop-set-window-height 50) ;the number for the percentage of the selected window. if 100, shell-pop use the whole of selected window, not spliting.
(shell-pop-set-window-position "bottom") ;shell-pop-up position. You can choose "top" or "bottom".
(global-set-key (kbd "C-x t") 'shell-pop)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified

(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers (or Gnus mail buffers)
					; Start Interactive Do
;; more info http://www.cua.dk/ido.html
(require 'ido)
(ido-mode 1)

(autoload 'autopair-global-mode "autopair" nil t)
(autopair-global-mode)
(add-hook 'lisp-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

;;(global-set-key (kbd "C-t") 'shell-pop)

(require 'naquadah-theme)

(setq mac-command-modifier 'meta)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)
(show-paren-mode)


;;Winner mode is this cool thing that lets you see previous states
;; of the buffer. Check it out!
(winner-mode)
(global-set-key (kbd "M-[") 'winner-undo)
(global-set-key (kbd "M-]") 'winner-redo)

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when isearch-forward (goto-char isearch-other-end)))

(global-set-key (kbd "C-c o") 'occur)

;; makes the mark visible. Try it out, it'll make a _ colored where
;; the mark is
(require 'visible-mark)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-;") 'jump-to-mark)

;;Make the eshell have the same directory as the current frame in emacs
(defun eshell-goto-current-dir (&optional arg)
  (interactive "P")
  (let ((dir default-directory))
    (shell-pop)
    (eshell/cd dir)))
(global-set-key "\C-x t" 'eshell-goto-current-dir)


;; All me, baby. Try it out. If theres a region, it'll kill that,
;; otherwise it'll just kill the word behind.
(defun backward-kill-word-or-kill-region (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (equal arg nil)
        (subword-backward-kill 1)
      (subword-backward-kill arg))))

(global-set-key (kbd "C-w") 'backward-kill-word-or-kill-region)
(setq initial-scratch-message nil)

;;shows a tree of undos in a seperate buffer. Use C-x u to visualize!
(require 'undo-tree)
(global-undo-tree-mode)

(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)

;; Popup everything in the
;; (defun push-mark-no-activate ()
;;   "Pushes `point' to `mark-ring' and does not activate the region
;;  Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
;;   (interactive)
;;   (push-mark (point) t nil)
;;   (message "Pushed mark to ring"))
;; (global-set-key (kbd "C-;") 'push-mark-no-activate)


;; Popup the kill ring, with string/subword searching.
(global-set-key "\M-y" 'popup-kill-ring)
(setq popup-kill-ring-interactive-insert t)

(transient-mark-mode 1)




(global-set-key '[C-tab] 'other-window)

;; One of the most useful things; used to keep your 80 char compliance
;; By putting up a bit line(here blue) at fill-column
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")
(setq fill-column 79)
(global-set-key (kbd "C-c C-t") 'fci-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(linum-mode 1)


;; Make ido complete everywhere
(icomplete-mode t)
(global-hl-line-mode t)
(mouse-wheel-mode t)
(setq-default indent-tabs-mode nil)

;; Set the line numbers
(global-linum-mode 1)

;; Move around windows with shift-arrow key. Other people like it, so I thought
;; I'd throw it in
(windmove-default-keybindings)

(setq show-paren-style 'mixed)

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

; Make emacs prefer to split horizantally
(setq split-width-threshold 9999)

(auto-fill-mode)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(defalias 'qrr 'query-replace-regexp)          ; Define alias for query
;; This command is AMAZING. I recommend mapping it to `C-x v' or `C-x w'
;; depending on which is easier on your keyboard.

(defun banish (l) (append (cdr l) (list (car l))))
(defun ido-jump-to-window ()
  (interactive)
  (let* ((visible-buffers (banish (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list))))
         (buffer-name (ido-completing-read "Window: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (setq window-of-buffer
                (delq nil (mapcar '(lambda (window)
                                       (if (equal buffer-name (buffer-name (window-buffer window)))
                                           window
                                         nil))
                                  (window-list))))
      (select-window (car window-of-buffer)))))

(define-key global-map (kbd "C-x v") 'ido-jump-to-window)


(defun enable-all-commands ()
      "Enable all commands, reporting on which were disabled."
      (interactive)
      (with-output-to-temp-buffer "*Commands that were disabled*"
        (mapatoms
         (function
          (lambda (symbol)
            (when (get symbol 'disabled)
              (put symbol 'disabled nil)
              (prin1 symbol)
              (princ "\n")))))))

(setq disabled-command-function nil)
(setq backup-directory-alist `(("." . "~/.emacs_backup")))

;; Turn on global-subword-mode, to recognize word boundaries
;; in camelCase
(global-subword-mode t)

;; Make a symbol appear where we last set the mark
(global-visible-mark-mode)

;; Automagically opened compressed files if I `Find file` them
(auto-compression-mode 1)


;; I leave emacs fullscreened a lot, so being able to see the time
;; in it is quite nice
(setq display-time-day-and-date t)
(display-time)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(defun setenv-from-shell (varname)
  (setenv varname (replace-regexp-in-string
                   "[ \t\n]*$"
                   ""
                   (shell-command-to-string (concat "$SHELL --login -i -c 'echo $" varname "'")))))

(setenv-from-shell "PYTHONPATH")


;; Maintain a list of recent files. C-x C-r to open from that list
(require 'recentf)

;; enable recent files mode.
(recentf-mode t)

; 1000 files ought to be enough.
(setq recentf-max-saved-items 1000)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

; Restore the state of emacs if it gets killed- no more whoops, killed emacs
; Also really handy since it'll save all those recently opened files across sessions now :)
(desktop-save-mode 1)

(setq tramp-default-method "ssh")

;; Dired and then some
(load-file "~/.emacs.d/tweaks/dired+.el")


;; ace-jump-mode. Allows you to go to a particular character with alacrity
(load-file "~/.emacs.d/tweaks/ace-jump-mode.el")
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

; Make ibuffer aware of vc stuff
(load-file "~/.emacs.d/tweaks/ibuffer-vc.el")