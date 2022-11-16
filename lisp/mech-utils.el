;; Run Powershell
;; Source: https://caiorss.github.io/Emacs-Elisp-Programming/Emacs_On_Windows.html
(defun run-powershell ()
  "Run Powershell"
  (interactive)
  (async-shell-command (concat (getenv "PROGRAMFILES") "\\powershell\\7\\pwsh.exe") nil nil))

;; Toggle mode line
(defun toggle-mode-line ()
  (interactive)
  (if mode-line-format
      (progn (defvar-local mode-line-format-saved mode-line-format) (setq mode-line-format nil))
    (progn (setq mode-line-format mode-line-format-saved) (redraw-display))))

;; Solarize theme
(defun load-theme-solarized()
  (add-to-list custom-theme-load-path "~/themes/")
  (load-theme 'solarized t)
  )

;; Insert zero-width space
(defun insert-zero-width-space ()
  (interactive)
  (insert-char #x200b))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-*") 'insert-zero-width-space))

;; Insert n columns table (vertical bar)
(defun mech-insert-table-ncolumns ()
  (interactive)
  (let (n)
    (setq n (read-number "Enter column number: " 3))
    (dotimes (number (1+ n)) (insert "| "))
    (backward-char (1+ (* n 2))))
  )

;; Rerun custom-set-faces after theme change
(defun mech-refresh-default-faces ()
  (interactive)
  (mech/font-face)
  (mech/org-colors)
  )

(defun mech-choose-fontsize ()
  (interactive)
  (let ((n (read-number "Enter fontsize: " 140)))
    (custom-set-faces
     `(default
	((t (:height ,n :family "Cascadia Code"))))
     `(fixed-pitch
       ((t (:height ,n :family "JetBrains Mono")))))
    )
  )

(provide 'mech-utils)
