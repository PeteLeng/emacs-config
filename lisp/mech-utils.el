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

(provide 'mech-utils)
