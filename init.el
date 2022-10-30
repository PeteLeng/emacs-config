;; -*- lexical-binding: t; -*-

;; Chemacs multiple profiles
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

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

;; UI, Completion
(defun mech/set-completion-styles ()
  (setq-local completion-styles '(basic partial-completion)))

(defun mech/after-init-hook ()
  (display-line-numbers-mode) ;; Source: https://emacs.stackexchange.com/a/280
  ;; (global-flycheck-mode)
  (global-company-mode)
  (vertico-mode)
  (savehist-mode) ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (projectile-mode)
  )

(defun mech/prog-mode-hook ()
  (hs-minor-mode)
  (flycheck-mode)
  ;; (mech/set-completion-styles)
  )

(defun mech/text-mode-hook ()
  (flyspell-mode 1)
  )

(defun mech/install-hooks ()
  (add-hook 'after-init-hook #'mech/after-init-hook)
  (add-hook 'prog-mode-hook #'mech/prog-mode-hook)
  (add-hook 'text-mode-hook #'mech/text-mode-hook)
  ;; (add-hook 'eshell-mode-hook #'mech/set-completion-styles)
  )

(defun mech/set-other ()
  (setq ring-bell-function 'ignore) ;; Source: https://emacs.stackexchange.com/a/28916
  )

;; Theme
(use-package gruvbox-theme
  ;; :disabled t
  :init (progn (load-theme 'gruvbox-dark-soft t t)
               (load-theme 'gruvbox-dark-medium t t)
               (load-theme 'gruvbox-dark-hard t t)
               (load-theme 'gruvbox-light-soft t t)
               (load-theme 'gruvbox-light-medium t t)
               (load-theme 'gruvbox-light-hard t t)
               (enable-theme 'gruvbox-dark-hard))
  )

;; Emacs
(use-package emacs
  :init
  (progn
    (mech/set-encoding)
    (mech/set-keybindings)
    (mech/set-fontset)
    (mech/set-font-face)
    (mech/set-minibuffer)
    (mech/set-display)
    (mech/tune-completion-performance)
    (mech/set-other)
    (mech/set-tramp)
    (mech/set-gdb)
    (mech/install-hooks)
    )
  )

;; Vertico completion
(use-package vertico
  :init
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Orderless matching
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(substring orderless basic)) ;; orderless overrides basic, partial completion
  ;; (setq completion-category-defaults nil)
  ;; Unclear what completion-category-overrides does
  ;; (setq completion-category-overrides '((file (styles . (orderless)))
  ;; 					(buffer (styles orderless))))
  )

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
  (global-set-key (kbd "M-\\") 'ispell-word)
  (global-set-key (kbd "C-:") 'flyspell-auto-correct-previous-word)
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

;; Org mode
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
  (mech/set-org-defaults)
  (mech/set-prettify-symbols-alist)
  (mech/set-org-latex)
  ;; To circumvent daemon and terminal mode
  (when (display-graphic-p) (mech/set-org-colors)) ;; Source: https://stackoverflow.com/a/5795518/17006775
  (mech/custom-org-agenda)
  (mech/set-org-agenda-face)
  (mech/install-org-hooks)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
	 ("M-S-<return>" . org-insert-todo-heading)
	 ("C-c i" . mech/org-capture-inbox)
         ))

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

;; Syntax checking
(use-package flycheck
  :config
  ;; Resolve keybinding collision from flycheck key prefix "C-c !"
  ;; https://stackoverflow.com/a/32239523/17006775
  (define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive)
  )

;; Auto-completion
(use-package company
  ;; Source: https://company-mode.github.io/
  :config
  ;; Inspired by post: https://emacs.stackexchange.com/q/14955
  ;; Also for debugging use M-x company-diag
  (push 'company-clang company-backends)
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.1)
  :bind
  (("C-<tab>" . company-complete))
  )

;; Project management
(use-package projectile
  :pin melpa
  ;; :init
  ;; (projectile-mode)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map)
	))

;; Python mode
;; (setq python-shell-interpreter "python")
(use-package elpy
  :pin melpa
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python")
  ;; (setq elpy-rpc-pythonpath "C:\\Users\\pete\\.emacs.d\\elpa\\elpy-20220322.41")
  )

;; C/C++ development
(defun mech/c-mode-hook ()
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
    (ggtags-mode 1)
    ;; (remove-hook 'completion-at-point-functions #'ggtags-completion-at-point) ;; remove outside hook does not work
    ))

(use-package ggtags
  :config
  (add-hook 'c-mode-common-hook #'mech/c-mode-hook)
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
  )

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :config
  ;; Inspired by post: https://emacs.stackexchange.com/a/22570
  (setq company-c-headers-path-system '("c:/msys64/usr/include/"
					"c:/msys64/mingw64/include/"
					"c:/msys64/mingw64/lib/clang/15.0.2/include/")))

(use-package sr-speedbar)

;; Trying to set local compleion styles for eshell, did not work
;; However, switching the order of completion styles works
;; Sources: https://github.com/purcell/emacs.d/issues/778
;; (use-package eshell
;;   :config
;;   (add-hook 'eshell-load-hook (lambda()
;; 				((setq-local completion-styles '(basic partial-completion)))))
;;   )

(global-set-key (kbd "C-c b") 'toggle-mode-line)

;; Daemon
;; Using Chemacs changes the server file path
(setq server-auth-dir "~/.emacs.d/server/")
(if (daemonp) (add-hook 'after-make-frame-functions
			(lambda (frame) (with-selected-frame frame
					  (progn (mech/set-fontset) (mech/set-org-colors))))))

(message "Successfully loaded Emacs Sigma!")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
