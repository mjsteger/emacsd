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
(setq split-width-threshold 999)

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

(enable-all-commands)

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

(load-file "~/.emacs.d/tweaks/mode-compile.el")
; Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
   "Command to compile current buffer file based on the major mode" t)
(global-set-key (kbd "C-c c") 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
 "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c k") 'mode-compile-kill)

(global-set-key (kbd "C-M-w") 'backward-kill-sexp)
(add-to-list 'load-path "~/.emacs.d/")
(require 'color-theme)
(require 'color-theme-tomorrow)
; (color-theme-tomorrow-night-eighties)
(load-file "~/.emacs.d/sr-speedbar.el")
(global-set-key (kbd "C-c g") 'sr-speedbar-toggle)


;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'font-lock-type-face) ","
      (propertize "%02c" 'face 'font-lock-type-face)
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))
    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    ))

(autopair-on)


;Save the place you were in a buffer, when you switch out/back in
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

(global-unset-key (kbd "<left>") )
(global-unset-key (kbd "<right>") )
(global-unset-key (kbd "<up>") )
(global-unset-key (kbd "<down>") )
(require 'edit-server)
(edit-server-start)
(setq edit-server-new-frame nil)
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("elisp" (or (filename . ".emacs.d")
                      (mode . emacs-lisp-mode)))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")))
         ("code" (mode . python-mode))
	 ("Web Dev" (or (mode . html-mode)
			(mode . css-mode)))
	 ("ERC" (mode . erc-mode))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "home")))

(setq ibuffer-expert t)
(setq ibuffer-auto-mode t)
(add-to-list 'load-path "~/.emacs.d/tweaks/bookmark+/")
(require 'bookmark+)
(require 'iedit)
(global-set-key (kbd "M-'") 'iedit-mode)
(global-set-key (kbd "C-x C-k") 'zap-to-char)
(setq emacs-directory "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/tweaks/backups-mode/")
(require 'backups-mode)
(load-file "~/.emacs.d/tweaks/load-rainbow.el")
(require 'keywiz)
(global-set-key (kbd "M-l") 'subword-mark)
(add-to-list 'load-path "~/.emacs.d/tweaks/emacs-color-theme-solarized/")
(require 'solarized-dark-theme)

(defadvice bookmark-write-file
  (after local-directory-bookmarks-to-zsh-advice activate)
  (local-directory-bookmarks-to-zsh))
(defun local-directory-bookmarks-to-zsh ()
  (interactive)
  (when (and (require 'tramp nil t)
             (require 'bookmark nil t))
    (set-buffer (find-file-noselect "~/.zsh.bmk" t t))
    (delete-region (point-min) (point-max))
    (insert "# -*- mode:sh -*-\n")
    (let (collect-names)
      (mapc (lambda (item)
              (let ((name (replace-regexp-in-string "-" "_" (car item)))
                    (file (cdr (assoc 'filename
                                      (if (cddr item) item (cadr item))))))
                (when (and (not (tramp-tramp-file-p file))
                           (file-directory-p file))
                  (setq collect-names (cons (concat "~" name) collect-names))
                  (insert (format "%s=\"%s\"\n" name (expand-file-name file) name)))))
            bookmark-alist)
      (insert ": " (mapconcat 'identity collect-names " ") "\n"))
    (let ((backup-inhibited t)) (save-buffer))
    (kill-buffer (current-buffer))))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
      '(search ring regexp-search-ring)    ;; ... my search entries
      savehist-autosave-interval 60        ;; save every minute (default: 5 min)
      savehist-file (concat "~/.emacs.d" "/savehist"))   ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation


;; ;; time-stamps
;; ;; when there's "Time-stamp: <>" in the first 10 lines of the file
;; (setq
;;  time-stamp-active t        ; do enable time-stamps
;;  time-stamp-line-limit 10   ; check first 10 buffer lines for Time-stamp: <>
;;  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
;; (add-hook 'write-file-hooks 'time-stamp) ; update when saving


;; ;; flyspell-mode
;; (setq ispell-program-name "aspell" ; use aspell instead of ispell
;;       ispell-extra-args '("--sug-mode=ultra"))
;; (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;; (add-hook 'message-mode-hook 'turn-on-flyspell)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)
;; (setq reb-re-syntax 'string)

(require 'paredit)


(autopair-global-mode)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
              (push '(?` . ?')
                    (getf autopair-extra-pairs :comment))
              (push '(?` . ?')
                    (getf autopair-extra-pairs :string))))

(add-hook 'haskell-mode
          '(lambda ()
              (push '(?` . ?')
                    (getf autopair-extra-pairs :comment))
              (push '(?` . ?')
                    (getf autopair-extra-pairs :string))))

(setq ido-create-new-buffer 'always)
(ac-flyspell-workaround)
(require 'auto-complete-yasnippet)
(setq shell-command-default-error-buffer "*error*")
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)
(iswitchb-mode)

;; Steal steal steal from Yegge


(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

;(global-set-key (kbd "C-i") 'swap-windows)


;;
;; Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil))))))

;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

(defalias 'rfab 'rename-file-and-buffer)
(defalias 'mbf 'move-buffer-file)
(add-hook 'js2-mode-hook
          '(lambda () (setq autopair-dont-activate t)))
