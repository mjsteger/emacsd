(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
					;(global-set-key "\C-cl" 'org-store-link)


(setq org-startup-indented t)
(require 'org-install)


(setq org-log-done t)
(setq org-agenda-files (list "~/org/devOpsTwilio.org"
                             "~/org/school.org"
                             "~/org/home.org"
			     "~/org/test.org"
			     "~/org/gtd.org"
			     "~/org/google.org"
			     "~/org/work.org"
			     "~/org/intern.twilio.org"
			     "~/org/refile.org"
			     ))


(setq org-capture-templates
      '(
	("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("w" "Devops" entry (file+headline "~/org/devOpsTwilio.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("n" "Note" entry (file "~/org/refile.org")
	 "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
	("a" "Interesting" entry (file "~/national/interesting.org")
	 "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
	("e" "Emacs" entry (file "~/org/emacs.org")
	 "* TODO %?\n  %i\n  %a" )
	("i" "Intern" entry (file+headline "~/org/intern.twilio.org" "Intern")
	 "* TODO %?\n %i\n %a")
	("h" "Home" entry (file+headline "~/org/home.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
	("s" "School" entry (file+headline "~/org/school.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))



;;; define categories that should be excluded
(setq org-export-exclude-category (list "google" "private"))



(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)")
              (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "yellow" :weight bold)
              ("STARTED" :foreground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "blue" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              )))




(require 'org-checklist)

					; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

					; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt)

(global-set-key (kbd "C-c g") 'org-timer-set-timer)
(setq org-agenda-start-on-weekday nil)
(setq org-timer-default-timer 6)
(setq orgPreviousWindow nil)
(setq org-default-notes-file "~/org/notes.org")

(load-file "~/.emacs.d/org-mode/clocking.el")

(defun org-alarm (txt)

(start-process "org-alarm" nil "play-sound" "~/Music/misc/alarm-clock-1.mp3")
)

(defun appt-display-message (string mins)
"appointmentg reminder for maemo"
(org-alarm (format "%s \nin %d min" string mins)))

(add-hook 'org-agenda-mode-hook
'(lambda nil (org-agenda-to-appt 'refresh "!")))
(appt-activate)



(add-hook 'org-mode-hook
	  (lambda() (add-hook 'before-save-hook
			      'org-agenda-to-appt t t)))


(require 'appt)
(setq appt-time-msg-list nil)
(setq appt-audible t)
(setq appt-display-format 'echo)
(appt-activate 1)
(setq appt-message-warning-time 15)  ; 12
;; update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
(setq appt-disp-window-function (function org-alarm))


(defvar alarm-clock-timer nil
   "Keep timer so that the user can cancel the alarm")


(defun alarm-clock-message (text)
"The actual alarm action"
(message-box text)
(start-process "org-alarm" nil "afplay" "~/Music/misc/alarm-clock-1.mp3")
)

(defun alarm-clock ()
  "Set an alarm.
The time format is the same accepted by `run-at-time'.  For
example \"11:30am\"."
  (interactive)
  (let ((time (read-string "Time: "))
        (text (read-string "Alarm message: ")))
    (setq alarm-clock-timer (run-at-time time nil 'alarm-clock-message
text))))

 (defun alarm-clock-cancel ()
  "Cancel the alarm clock"
  (interactive)
  (cancel-timer alarm-clock-timer))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Notes and Tasks to Refile")
                       (org-agenda-overriding-header "Tasks to Refile")))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                (tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-WAITING"
                           ((org-agenda-overriding-header "Relevant Tasks")
                            (org-agenda-skip-function 'bh/skip-non-relevant-tasks)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-todo-ignore-deadlines 'future)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (todo "WAITING|SOMEDAY"
                      ((org-agenda-overriding-header "Waiting and Postponed tasks")
                       (org-agenda-skip-function 'bh/skip-projects)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))
               nil)
              ("r" "Tasks to Refile" tags "REFILE"
               ((org-agenda-overriding-header "Notes and Tasks to Refile")
                (org-agenda-overriding-header "Tasks to Refile")))
              ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Stuck Projects")
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
              ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT|STARTED"
               ((org-agenda-overriding-header "Next Tasks")
                (org-agenda-skip-function 'bh/skip-projects)
                (org-tags-match-list-sublevels t)
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              ("R" "Relevant Tasks" tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-WAITING"
               ((org-agenda-overriding-header "Relevant Tasks")
                (org-agenda-skip-function 'bh/skip-non-relevant-tasks)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("p" "Projects" tags-todo "-CANCELLED/!"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-skip-function 'bh/skip-non-projects)
                (org-tags-match-list-sublevels 'indented)
                (org-agenda-todo-ignore-scheduled 'future)
                (org-agenda-todo-ignore-deadlines 'future)
                (org-agenda-sorting-strategy
                 '(category-keep))))
              ("w" "Waiting Tasks" todo "WAITING|SOMEDAY"
               ((org-agenda-overriding-header "Waiting and Postponed tasks"))
               (org-agenda-skip-function 'bh/skip-projects))
              ("A" "Tasks to Archive" tags "-REFILE/"
               ((org-agenda-overriding-header "Tasks to Archive")
                (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))))


; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")


(setq org-hide-leading-stars nil)

(setq org-reverse-note-order nil)

(setq org-stuck-projects (quote ("" nil nil "")))


(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask and is not a subtask of another project
This does not support projects with subprojects"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-subproject (bh/is-subproject-p))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and is-a-task has-subtask (not is-subproject))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(NEXT\\|STARTED\\) " subtree-end t)))))
    (if (and (bh/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      next-headline)))

(defun bh/skip-non-project-trees ()
  "Skip trees that are not projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        nil
      subtree-end)))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (cond
     ((bh/is-project-p)
      subtree-end)
     ((org-is-habit-p)
      subtree-end)
     (t
      nil))))

(defun bh/skip-projects ()
  "Skip trees that are projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (cond
     ((bh/is-project-p)
      next-headline)
     (t
      nil))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (cond
     ((bh/is-project-p)
      next-headline)
     ((org-is-habit-p)
      next-headline)
     (t
      nil))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING"))
              ("STARTED" ("WAITING"))
              ("DONE" ("WAITING") ("CANCELLED")))))

(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
	    ;; auto-fill mode on
            (auto-fill-mode 1)))


;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
              (todo category-up priority-down effort-up)
              (tags category-up priority-down effort-up)
              (search category-up))))

;; Start the weekly agenda today
(setq org-agenda-start-on-weekday nil)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote((daily today remove-match)
                                  #("----------------" 0 16
                                    (org-heading t))
                                  (800 1000 1200 1400 1600 1800 2000))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '< a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\.:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))


(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/org/diary.org")

(setq org-agenda-insert-diary-extract-time t)
(setq org-return-follows-link t)