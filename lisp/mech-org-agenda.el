;; Utility functions
(defun dels (str str-list)
  (seq-remove (lambda (itm) (equal itm str)) str-list))
(defun delss (str-set str-list)
  (seq-remove (lambda (itm) (member itm str-set)) str-list))
(defun delvs (str-list &rest regs)
  (seq-remove (lambda (str) (cl-some (lambda (reg) (string-match reg str)) regs)) str-list))

;; Custom Org Agenda
(defun mech/custom-org-agenda()
    ;; Agenda files
    ;; the use of wildcard is suggested at:
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2009-10/msg00734.html
    ;; (setq org-agenda-files (file-expand-wildcards "~/org/vault/*inbox.org"))
    (setq mech/org-agenda-dir "~/org/gtd/")
    (setq org-agenda-files (file-expand-wildcards (concat mech/org-agenda-dir "*.org")))
    
    ;; Capture templates
    (setq org-capture-templates
	  `(("i" "Inbox" entry  (file ,(concat mech/org-agenda-dir "inbox.org"))
             "* TODO %?\n") ;; "/Entered on/ %U" recorded under the PROPERTIES drawer
	    ("p" "Project" entry (file ,(concat mech/org-agenda-dir "projects.org"))
	     "* %? [/]")))
    (setq org-refile-targets
	  `((,(dels "~/org/gtd/inbox.org" org-agenda-files) :level . 1)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil) ;; complete in one step when using completion package

    ;; TODO
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))
    (setq org-log-done 'time) ;; Log time when task is marked DONE
    
    ;; Agenda view
    ;; (setq org-agenda-hide-tags-regexp ".") ;; Hide tags
    (defun decode-time-diff (t1 t2)
      (let* ((time-diff (time-convert (time-subtract t1 t2) 'integer))
	     (sign 0))
	(if (< time-diff 0) (progn (setq time-diff (- time-diff)) (setq sign 1)))
	(setq d (/ (/ time-diff 3600) 24))
	(setq h (mod (/ time-diff 3600) 24))
	(setq m (/ (mod time-diff 3600) 60))
	(list sign d h m)))
    
    (defun mech/agenda-prefix ()
      (let* ((deadline (org-get-deadline-time (point)))
	     (appt (org-get-scheduled-time (point))))
	;; (if deadline (format-time-string "%R" deadline) "")
	(if deadline
	    (progn
	      (setq delta (decode-time-diff (current-time) deadline))
	      (if (eq 1 (car delta))
		  (format "%02dd %02dh %02dm till due " (nth 1 delta) (nth 2 delta) (nth 3 delta))
		(format "%02dd %02dh %02dm past due " (nth 1 delta) (nth 2 delta) (nth 3 delta))))
	  (if appt
	      (progn
		(setq delta (decode-time-diff (current-time) appt))
		(if (eq 1 (car delta))
		    (format "%02dd %02dh %02dm till " (nth 1 delta) (nth 2 delta) (nth 3 delta))
		  ""))
	    ""))))

    (setq mech/org-agenda-custom-view
	  `(("g" "Save your dream!!!"
	    ((agenda ""
                     ((org-agenda-overriding-header "Today's Schedule")
		      (org-agenda-span 1)
                      (org-deadline-warning-days 14)
		      (org-agenda-files (delvs (file-expand-wildcards "~/org/gtd/*.org") "someday"))
		      (org-agenda-prefix-format " %i %?-12t% T%(mech/agenda-prefix)")))
	     (todo "NEXT"
		   ((org-agenda-overriding-header "In Progress")
                    (org-agenda-files (delvs (file-expand-wildcards "~/org/gtd/*.org") "someday" "calendar"))))
             (todo "TODO"
		   ((org-agenda-overriding-header "Projects")
                    (org-agenda-files '(,(concat mech/org-agenda-dir "projects.org")))
		    (org-agenda-prefix-format " %i %? b%?-6 e%? T")
                    ))
	     (todo "TODO"
		   ((org-agenda-overriding-header "Inbox")
                    (org-agenda-files (delvs (file-expand-wildcards "~/org/gtd/*.org") "someday" "calendar" "projects" "next"))))
	     (agenda "Week At A Glance"
                     ((org-agenda-overriding-header "Week At A Glance")
		      (org-agenda-span 7)
                      (org-deadline-warning-days 0)
		      (org-agenda-files (delvs (file-expand-wildcards "~/org/gtd/*.org") "someday"))
		      (org-agenda-prefix-format " %i %?-12t% T%(mech/agenda-prefix)")))
             (todo "TODO"
		   ((org-agenda-overriding-header "One-off Tasks")
                    (org-agenda-files '(,(concat mech/org-agenda-dir "next.org")))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))
	    ;; ((org-agenda-compact-blocks t))
	    ))
	  )
    (setq org-agenda-custom-commands `,mech/org-agenda-custom-view)
    )

;; Custom Org Agenda Faces
(defun mech/org-agenda-face ()
  ;; M-x describe-face gets the face at point
  (custom-set-faces
   `(org-drawer
     ((t (:foreground "wheat4" :height 120))))
   `(org-special-keyword
     ((t (:foreground "wheat4" :height 120))))
   `(org-agenda-structure
     ((t (:underline t :foreground "sienna2" :family "JetBrains Mono"))))
   `(org-agenda-date
     ((t (:foreground "sienna4" :family "JetBrains Mono"))))
   `(org-scheduled
     ((t (:foreground "wheat4")))))
  )

;; Custom keybinding for capture
(defun mech/org-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

;; Custom Capture hook
(defun mech/org-capture-log (&rest ignore)
  (message "Start mech/log in org-capture-before-finalize-hook")
  ;; Org roam node find abuses the org capture function
  ;; Org roam node typically does not have headings, causing org-entry-get to throw error
  (catch 'roam-capt
    ;; Source: (org-roam-capture-p)
    (if (plist-get org-capture-plist :org-roam)
	(progn
	  (message "This is org roam capture. Exit")
	  (throw 'roam-capt t)))
    (message "The org entry heading: %s" (org-entry-get nil "ITEM"))
    
    (save-excursion
      ;; Source on getting the heading of an entry: https://emacs.stackexchange.com/a/29884
      ;; Test if this is a TODO heading
      (when (and (org-get-todo-state) (not (org-entry-get (point) "CREATED")))
	;; Example ts: [2022-10-13 Thu 16:39]
	;; (message "%s" "Log creation time of TODO")
	(progn
	  (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %R]"))
	  (org-entry-put nil "CONTEXT" (if (null org-stored-links) "None."
					 (format "\[\[%s\]\]" (car (pop org-stored-links)))))))
      ;; Return point to the top heading
      (while (org-up-heading-safe)
	(message "Move up heading."))
      ;; Test if top heading is a project entry.
      (when (and (string-match "\[./.\]" (org-entry-get nil "ITEM")) (not (org-entry-get (point) "CREATED")))
	(progn
	  (message "String matches a project")
	  (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %R]"))
	  (org-entry-put nil "COOKIE_DATA" "recursive todo")
	  (org-entry-put nil "CONTEXT" (if (null org-stored-links) "None."
					 (format "\[\[%s\]\]" (car (pop org-stored-links))))))))
    (end-of-line))
  (message "Finish mech/log in org-capture-before-finalize-hook"))

(provide 'mech-org-agenda)
