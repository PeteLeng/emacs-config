;; Custom prettify symbols
(defun mech/set-prettify-symbols-alist ()
  (setq-default prettify-symbols-alist
                '(("#+BEGIN_SRC" . "❮")
                  ("#+END_SRC" . "❯")
                  ("#+begin_src" . "❮")
                  ("#+end_src" . "❯")
                  ;; ("#+begin_quote" . "❝")
                  ;; ("#+end_quote" . "❞")
                  ("#+begin_quote" . "“")
                  ("#+end_quote" . "”")
                  ("#+begin_verse" . "“")
                  ("#+end_verse" . "”")
                  (">=" . "≥")
                  ("<=" . "≤")
                  ("->" . "→")
                  ("<-" . "←")
                  ("=>" . "⇒")
                  ("<=" . "⇐")
                  ("!=" . "≠")
                  ))
  )

;; Custom Org defaults
(defun mech/set-org-defaults ()
  (progn
    (setq org-hide-leading-stars t)
    (setq org-fontify-done-headline t)
    (setq org-hide-emphasis-markers t)
    ;; (setq org-pretty-entities t)
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    (setq org-cycle-include-plain-lists 'integrate)     ;; Collapse plain lists
    (setq org-image-actual-width nil)			;; Resize pictures with captions
    (setcdr (car (last org-file-apps)) 'emacs))		;; Set emacs as default pdf reader
							;; Breaks Latex preview potentially
  )

;; Custom Org Latex
(defun mech/set-org-latex ()
  ;; (plist-put org-format-latex-options :scale 2) ;; Set directly in imagemagick flags
  (setq org-preview-latex-default-process 'imagemagick)
  ;; (setq org-startup-with-latex-preview t)
  (add-to-list 'org-preview-latex-process-alist
               '(imagemagick :programs ("latex" "convert")
                             :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                             (2.2 . 2.2)
                             :latex-compiler
                             ("pdflatex -interaction nonstopmode -output-directory %o %f")
                             :image-converter
                             ("magick convert -density %D -trim -antialias %f -quality 100 %O"))))

;; Custom Org colors
(defun mech/set-org-colors ()
  (require 'color)
  ;; Code blocks
  (setq code-block-bg-color (color-darken-name (face-attribute 'default :background) 5))
  
  ;; Quote blocks bg
  ;; Source: https://emacs.stackexchange.com/a/41260
  (setq org-fontify-quote-and-verse-blocks t)

  ;; Links
  ;; Obsidian purple "#5b45bd"
  (setq link-fg-color "SlateGrey")

  ;; Inline code
  (setq inline-code-bg-color (color-darken-name (face-attribute 'default :background) 3))
  (setq inline-code-fg-color "firebrick") ;; "#c7254e"

  ;; Headings
  ;; Source: https://github.com/greduan/emacs-theme-gruvbox
  ;; (setq header-color-set-dark-theme '("#9d8469" "#a79178" "#b19d88" "#bbaa97" "#c5b7a6" "#cfc3b6" "#d9d0c5" "#d9d0c5"))
  (progn
  (setq head-base-color "DarkOliveGreen")
  (setq i 0)
  (setq colors '())
  (while (< i 8)
    (setq i (1+ i))
    (setq color (color-lighten-name head-base-color (* 5 i)))
    (setq colors (append colors (list color))))
  (setq i 0)
  (dolist (heading org-level-faces)
    (setq i (1+ i))
    (custom-set-faces
     `(,heading ((t (:foreground ,(elt colors i))))))))
  
  (custom-set-faces
   `(org-block-begin-line
     ((t (:background ,code-block-bg-color :extend t))))
   `(org-block
     ((t (:background ,code-block-bg-color :extend t))))
   `(org-block-end-line
     ((t (:background ,code-block-bg-color :extend t))))
   `(org-code
     ((t (:family "Fantasque Sans Mono" :background ,inline-code-bg-color :foreground ,inline-code-fg-color))))
   `(org-link
     ((t (:foreground ,link-fg-color :underline t))))
   ))

(provide 'mech-org)
