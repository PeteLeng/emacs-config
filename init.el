;; -*- lexical-binding: t; -*-

;; Chemacs multiple profiles
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
(setq package-gnupghome-dir "c:/Users/pete/.emacs-profs/.emacs.prime/elpa/gnupg")

;; Package management
;; Sources: https://ianyepan.github.io/posts/setting-up-use-package/
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Add custom file path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'mech-defaults)
(require 'mech-utils)
(require 'mech-org)
(require 'mech-org-agenda)

;; Custom hooks
(defun mech/after-init-hook ()
  (global-company-mode)
  (savehist-mode) ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (vertico-mode)
  (which-key-mode)
  (projectile-mode)
  (yas-global-mode)
  )

(defun mech/prog-mode-hook ()
  ;; (display-line-numbers-mode) ;; Source: https://emacs.stackexchange.com/a/280
  (linum-mode)
  (hs-minor-mode)
  (flycheck-mode)
  )

(defun mech/text-mode-hook ()
  ;; (display-line-numbers-mode) ;; Source: https://emacs.stackexchange.com/a/280
  (linum-mode)  
  (flyspell-mode 1)
  )

(defun mech/eshell-mode-hook ()
  (setq-local completion-styles '(basic substring orderless))
  )

(defun mech/install-hooks ()
  (add-hook 'after-init-hook #'mech/after-init-hook)
  (add-hook 'prog-mode-hook #'mech/prog-mode-hook)
  (add-hook 'text-mode-hook #'mech/text-mode-hook)
  (add-hook 'eshell-mode-hook #'mech/eshell-mode-hook)
  )

(defun mech/other ()
  (setq ring-bell-function 'ignore) ;; Source: https://emacs.stackexchange.com/a/28916
  )

;; Theme
(use-package gruvbox-theme
  :init (progn (load-theme 'gruvbox-dark-soft t t)
               (load-theme 'gruvbox-dark-medium t t)
               (load-theme 'gruvbox-dark-hard t t)
               (load-theme 'gruvbox-light-soft t t)
               (load-theme 'gruvbox-light-medium t t)
               (load-theme 'gruvbox-light-hard t t)
               (enable-theme 'gruvbox-dark-hard))
  )

;; Completion
(use-package orderless
  :init
  (setq completion-styles '(substring orderless basic))
  )

;; Vertico completion
(use-package vertico
  :ensure t
  :init

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  ;; Enable vertico-multiform
  (vertico-multiform-mode)

  ;; Configure the display per completion category.
  ;; Use the grid display for files and a buffer
  ;; for the consult-grep commands.
  ;; (setq vertico-multiform-categories '((file grid) (consult-grep buffer)))
  )

;; Which key
(use-package which-key
  :config
  (setq which-key-popup-type 'minibuffer))

;; Mini-Buffer Actions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Source: https://www.reddit.com/r/emacs/comments/osx5t9/comment/h6rkbcx/?utm_source=share&utm_medium=web2x&context=3
  ;; (custom-set-variables '(embark-verbose-indicator-display-action '(display-buffer-below-selected (window-height . 12))))

  ;; which-key intergration
  ;; Source: https://github.com/oantolin/embark/wiki/Additional-Configuration
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators '(embark-which-key-indicator embark-highlight-indicator embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter :around #'embark-hide-which-key-indicator)
  (set-face-attribute 'embark-verbose-indicator-title nil :height 1.0)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Spell check
(use-package flyspell
  :init
  (progn
    (setq ispell-program-name "hunspell")
    (setq ispell-hunspell-dict-paths-alist '(("en_US" "C:/Users/pete/LibreDict/en_US.aff")))
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
    )
  :config
  ;; resolve collision between flyspell and embark
  (global-set-key (kbd "C-~") flyspell-mode-map)
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (global-set-key (kbd "M-\\") 'ispell-word)
  )

;; PDF mode
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  ;; :pin manual ;; use-package cannot load pdf-tools
  ;; https://www.reddit.com/r/emacs/comments/qn6lv9/pdftools_melpa_problem/?utm_source=share&utm_medium=web2x&context=3
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; (setq pdf-annot-activate-created-annotations t)

  ;; Source: https://pragmaticemacs.wordpress.com/2017/11/08/more-pdf-tools-tweaks/
  ;; fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  )

;; Allow org to link to specific annotations
(use-package org-pdftools
  :ensure t
  ;; :hook (org-mode . org-pdftools-setup-link)
  )

;; Snippets
(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

;; Python Virtual Environment
(defun mech/set-python-interpreter()
  (setq python-shell-interpreter (concat pyvenv-virtual-env "Scripts/python")))

(defun mech/unset-python-interpreter()
  (setq python-shell-interpreter "python3"))

(use-package pyvenv
  :ensure t
  ;; :init
  ;; (setenv "WORKON_HOME" "~/.pyenv/versions")
  :config
  ;; Changing name from python3 to python invokes the correct python interpreter in virtual environment.
  ;; Inspired by source: https://blog.fredrikmeyer.net/2020/08/26/emacs-python-venv.html
  (setq python-shell-interpreter "python") ;; Original value is python3
  )

;; CC mode
(use-package cc-mode
  :config
  (c-set-offset 'case-label '+)
  )

;; Rust mode
(use-package rustic)

;; LSP
(use-package lsp-mode
  :ensure t
  :config
  ;; (add-hook 'prog-mode-hook #'lsp-deferred)
  (setq lsp-signature-render-documentation nil)
  
  ;; Remote clangd
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  )

;; Git intergration
(use-package magit
  :ensure t)

;; Syntax checking
(use-package flycheck
  :ensure t
  :config
  ;; Resolve keybinding collision from flycheck key prefix "C-c !"
  ;; https://stackoverflow.com/a/32239523/17006775
  (define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive)
  )

;; Auto-completion
(use-package company
  :ensure t
  ;; Source: https://company-mode.github.io/
  :config
  ;; Inspired by post: https://emacs.stackexchange.com/q/14955
  ;; Use M-x company-diag for debugging.
  (setq company-minimum-prefix-length 2)
  ;; (setq company-idle-delay 0.2)
  ;; Sources: https://stackoverflow.com/a/11573802/17006775
  (setq company-backends (remove 'company-clang company-backends))
  )

;; Expand region
(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  )

;; Project management
(use-package projectile
  :pin melpa
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)
	))

;; File structure visualization
(use-package treemacs
  :ensure t
  :defer t
  :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; Ace-window
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-dispatch-always t)
  ;; Source: https://oremacs.com/2015/02/27/ace-window-leading-char/
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 1.0)))))
  :bind
  (("M-o" . ace-window)))

;; Emacs
(use-package emacs
  :init
  (progn
    (mech/encoding)
    (mech/keybindings)
    (mech/fontset)
    (mech/font-face)
    (mech/minibuffer)
    (mech/display)
    (mech/completion-behav)
    (mech/other)
    (mech/tramp)
    (mech/gdb)
    (mech/install-hooks)
    )
  )

;; Org mode
(use-package org-appear
  :config
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-inside-latex t)
  )

(use-package org-bullets
  :custom
  (org-ellipsis "⤵")
  :config
  (setq org-bullets-bullet-list '("◉" "∙" "∙" "∙" "∙" "∙" "∙" "∙")))

(defun mech/org-mode-hook ()
  (progn
    (visual-line-mode)
    (org-indent-mode)
    (prettify-symbols-mode)
    (org-bullets-mode 1)
    (org-appear-mode)
    (org-pdftools-setup-link)
    ))

(defun mech/install-org-hooks ()
  (add-hook 'org-mode-hook #'mech/org-mode-hook)
  (add-hook 'org-capture-before-finalize-hook #'mech/org-capture-log)
  (advice-add 'org-insert-todo-heading :after #'mech/org-capture-log)
  (advice-add 'org-insert-todo-heading-respect-content :after #'mech/org-capture-log)
  (advice-add 'org-insert-todo-subheading :after #'mech/org-capture-log)
  )

(use-package org
  :config
  (mech/org-defaults)
  (mech/prettify-symbols-alist)
  (mech/org-latex)
  ;; To circumvent daemon and terminal mode
  (when (display-graphic-p) (mech/org-colors)) ;; Source: https://stackoverflow.com/a/5795518/17006775
  (mech/custom-org-agenda)
  (mech/org-agenda-face)
  (mech/install-org-hooks)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
	 ("M-S-<return>" . org-insert-todo-heading)
	 ("C-c i" . mech/org-capture-inbox)
         ))

;; Org roam
(use-package org-roam
  :after org
  :init
  (setq org-directory (concat (getenv "HOME") "\\org\\vault"))
  (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom (org-roam-directory (file-truename org-directory))
  :config
  ;; (org-roam-setup) ; This function is obsolete.
  (org-roam-db-autosync-enable)
  ;; (setq org-roam-database-connector 'sqlite3)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-c n k" . org-id-get-create)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n b" . org-roam-buffer-toggle))
  )

;; Keybinding for utility functions
(global-set-key (kbd "C-c b") 'toggle-mode-line)

;; Daemon
;; Using Chemacs changes the server file path
(setq server-auth-dir "~/.emacs.d/server/")
(if (daemonp) (add-hook 'after-make-frame-functions
			(lambda (frame) (with-selected-frame frame
					  (progn (mech/fontset) (mech/org-colors))))))

(message "Welcome to Emacs Prime!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" default))
 '(embark-verbose-indicator-display-action '(display-buffer-below-selected (window-height . 16)))
 '(package-selected-packages
   '(which-key expand-region abc treemacs projectile company flycheck lsp-mode pyvenv yasnippet-snippets yasnippet org-roam org-bullets org-appear vertico use-package org-pdftools orderless gruvbox-theme embark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Cascadia Code"))))
 '(fixed-pitch ((t (:height 130 :family "JetBrains Mono"))))
 '(org-agenda-date ((t (:foreground "sienna4" :family "JetBrains Mono"))))
 '(org-agenda-structure ((t (:underline t :foreground "sienna2" :family "JetBrains Mono"))))
 '(org-block ((t (:background "#112312e91380" :extend t))))
 '(org-block-begin-line ((t (:background "#112312e91380" :extend t))))
 '(org-block-end-line ((t (:background "#112312e91380" :extend t))))
 '(org-code ((t (:family "Fantasque Sans Mono" :background "#112312e91380" :foreground "firebrick"))))
 '(org-drawer ((t (:foreground "wheat4" :height 120))))
 '(org-level-1 ((t (:foreground "#b3c05bd03123"))))
 '(org-level-2 ((t (:foreground "#c7da661536a2"))))
 '(org-level-3 ((t (:foreground "#ce35751d49e1"))))
 '(org-level-4 ((t (:foreground "#d3b484725dfb"))))
 '(org-level-5 ((t (:foreground "#d93393c77216"))))
 '(org-level-6 ((t (:foreground "#deb2a31b8631"))))
 '(org-level-7 ((t (:foreground "#e431b2709a4b"))))
 '(org-level-8 ((t (:foreground nil))))
 '(org-link ((t (:foreground "SlateGrey" :underline t))))
 '(org-scheduled ((t (:foreground "wheat4"))))
 '(org-special-keyword ((t (:foreground "wheat4" :height 120)))))
