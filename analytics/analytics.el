(defgroup analytics nil
"The Analytics platform that Mikey is writing"
)

(defcustom function-state-file "~/.functions"
  "file used to persist the functions and placement across sessions"
  :type 'file
  :group 'analytics)

(defcustom keystroke-state-file "~/.keystrokes"
  "File used to persist the keystrokes across sessions"
  :type 'file
  :group 'analytics)

(defvar *analytics-save-keystrokes-timer nil
"Timer so that we send the keystrokes every so often"
)
(defvar *functions-count #s(hash-table size 1000 test equal data ()))
(defvar *keystrokes-count #s(hash-table size 2000 test equal data()))

(defun load-hash (file variable)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (setq variable (eval (buffer-string))))))

(defun save-hash (file variable)
  (when (not(equal variable nil))
    (with-temp-buffer
      (insert (format "%s" variable))
      (write-region (point-min) (point-max) (eval file)))))

(defun add-keystroke (variable command)
  (puthash command (+ 1 (gethash command variable 0)) variable))
(add-hook 'post-command-hook 'analytics-post-command-hook)

;;(add-hook 'post-self-insert-hook 'analytics-post-insert-hook)

(defun analytics-post-command-hook ()
  (if (not(equal this-command 'self-insert-command))
      (add-keystroke *functions-count this-command)
    (add-keystroke *keystrokes-count last-command-event))

  )
;; (defun analytics-post-insert-hook ()
;;   (add-keystroke *keystrokes-count last-command-event))

(save-hash keystroke-state-file *keystrokes-count)
(defun send-keystrokes ()
  (run-with-timer 1 nil 'save-hash function-state-file *functions-count)
  (run-with-timer 1 nil 'save-hash keystroke-state-file *keystrokes-count)
)

(provide 'analytics)
(clrhash *keystrokes-count)
(send-keystrokes)


