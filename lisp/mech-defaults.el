;; Custom Encoding
(defun mech/set-encoding()
  (set-language-environment 'utf-8)
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  )

;; Custom Key-bindings
(defun mech/set-keybindings ()
  ;; windows key to super key
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-])

  ;; Navigate paragraphs
  (global-set-key (kbd "M-p") 'backward-paragraph)
  (global-set-key (kbd "M-n") 'forward-paragraph)

  ;; Navigate balanced expressions
  ;; (global-set-key (kbd "M-a") 'backward-sexp)
  ;; (global-set-key (kbd "M-e") 'forward-sexp)

  ;; Resize windows
  (global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "s-<up>") 'shrink-window)
  (global-set-key (kbd "s-<down>") 'enlarge-window)

  ;; Comment line
  (global-set-key (kbd "C-;") 'comment-line)

  ;; Evaluate expression
  (global-set-key (kbd "C-=") 'eval-last-sexp)

  ;; Undo
  (global-set-key (kbd "C-z") 'undo)

  ;; Visible mode, for easy editting in when the org-hide-emphasis-markers is turned on
  (global-set-key (kbd "C-c v") 'visible-mode)

  ;; Mark symbolic expression
  (global-set-key (kbd "M-#") 'mark-sexp)

  ;; Imenu
  (global-set-key (kbd "C-c m") 'imenu)

  ;; Search
  (global-set-key (kbd "M-s s") 'isearch-forward-thing-at-point)

  ;; Hide-show minor mod
  ;; Source: https://emacs.stackexchange.com/a/33686
  (with-eval-after-load "hideshow"
    ;; swap @ with h
    (define-key hs-minor-mode-map (kbd "C-c h")
      (lookup-key hs-minor-mode-map (kbd "C-c @")))
    ;; disable @
    (define-key hs-minor-mode-map (kbd "C-c @") nil))
  )

;; Custom Fontset
(defun mech/set-fontset()
  (set-fontset-font t 'han
                    (cond
                     ((string-equal system-type "windows-nt")
                      (cond ((member "Noto Sans Mono CJK SC" (font-family-list)) "Noto Sans Mono CJK SC")))
                     ((string-equal system-type "gnu/linux")
                      (cond ((member "Noto Sans Mono CJK SC" (font-family-list)) "Noto Sans Mono CJK SC")))))

  (set-fontset-font t 'symbol
                    (cond
                     ((string-equal system-type "windows-nt")
                      (cond ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")))
                     ((string-equal system-type "gnu/linux")
                      (cond ((member "Symbola" (font-family-list)) "Symbola")))))

  (set-fontset-font t (if (version< emacs-version "28.1") '(#x1f300 . #x1fad0) 'emoji)
                    (cond ((member "Noto Emoji" (font-family-list)) "Noto Emoji")))
  )

;; Custom Font
(defun mech/set-font-face()
  (custom-set-faces
   `(default
      ((t (:height 140 :family "Cascadia Code"))))
   `(fixed-pitch
     ((t (:height 140 :family "JetBrains Mono"))))))

;; Custom Minibuffer
;; Source:
(defun mech/set-minibuffer ()
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;; I disabled this part because I did not understand the code.
  ;; (defun crm-indicator (args)
  ;;   (cons (format "[CRM%s] %s"
  ;;                 (replace-regexp-in-string
  ;;                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;                  crm-separator)
  ;;                 (car args))
  ;;         (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  )

;; Custom UI
(defun mech/set-display ()
  (tool-bar-mode 0)
  (menu-bar-mode 1)
  (set-scroll-bar-mode nil)
  (setq inhibit-startup-message t)
  ;; (setq-default line-spacing 1) ;; Leads to image flickering
  (electric-pair-mode)
  (setq split-height-threshold nil)
  )

;; Completion Performance
(defun mech/set-completion-behav ()
  (setq gc-cons-threshold 12800000)
  (setq read-process-output-max (* 64 1024))
  (setq read-file-name-completion-ignore-case t
	  read-buffer-completion-ignore-case t
	  completion-ignore-case t)
  )

;; Configure gdb view
(defun mech/set-gdb ()
  (setq gdb-many-windows t)
  (setq gdb-show-main t)
  )

;; Configure tramp
(defun mech/set-tramp ()
  (require 'tramp)
  (setq tramp-default-method "plink")
  ;; Source of configuring tramp-methods: https://emacs.stackexchange.com/a/52365.
  ;; Spent three hours on this, can't believe this just worked.
  (add-to-list 'tramp-methods
               `("plinkw"
                 (tramp-login-program        "plink")
                 ;; ("%h") must be a single element, see `tramp-compute-multi-hops'.
                 (tramp-login-args           (("-l" "%u") ("-P" "%p") ("-t")
                                              ("%h") ("\"")
                                              (,(format
                                                 "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
                                                 tramp-terminal-type
                                                 "$"))
                                              ("/bin/sh") ("\"")))
                 (tramp-remote-shell         "/bin/sh")
                 (tramp-remote-shell-login   ("-l"))
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-default-port         22))
               )
  )

(provide 'mech-defaults)
