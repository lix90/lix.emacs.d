(use-package flyspell-popup :ensure t :defer t :after flyspell
  :config (bind-keys :map flyspell-mode-map ("±" . flyspell-popup-correct)))

(use-package flyspell :ensure t :defer t
  :init (defun flyspell-toggle ()
          (interactive)
          (if flyspell-mode (flyspell-mode-off) (flyspell-mode)))
  :config
  (setq ispell-dictionary "english")
  (dolist (hook '(text-mode-hook))
	(add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
	(add-hook hook (lambda () (flyspell-mode -1))))
  (advice-add 'flyspell-mode-on :before 'flyspell-buffer))

;;; mmm
(use-package mmm-mode :ensure t :defer t
  :init
  (mmm-add-classes
   '((markdown-python
      :submode python-mode
      :face mmm-declaration-submode-face
      :front "^```python[\n\r]+"
      :back "^```$")))
  (mmm-add-mode-ext-class 'markdown-mode nil 'markdown-python))

;;; markdown
(use-package markdown-mode :ensure t :defer t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode))
  :init
  ;; markdown hooks
  (add-hook 'markdown-mode-hook
            '(lambda()
               (linum-mode)
               (mmm-mode +1)
               ;;(centered-cursor-mode)
               ;;(flyspell-mode 1)
               ;;(pandoc-mode)
               (hl-todo-mode)
               (git-gutter+-mode t)))
  (setq markdown-command "pandoc"
        markdown-enable-math t
        ;; markdown-footnote-location "end"
        markdown-nested-imenu-heading-index t
        ;;markdown-open-command "/Users/Roambot/bin/scripts/mark.sh"
        )
  :config  
  (progn 
    
    (general-define-key :states '(normal) :keymaps 'markdown-mode-map
                        "TAB" 'markdown-cycle
                        "gj"  'outline-forward-same-level
                        "gk"  'outline-backward-same-level
                        "gh"  'outline-up-heading
                        ;; next visible heading is not exactly what we want but close enough
                        "gl"  'outline-next-visible-heading)
    ;; "<return>" 'markdown-jump
    ;; Promotion, Demotion
    (general-define-key :states '(normal insert emacs) :keymaps 'markdown-mode-map
                        "M-h" 'markdown-promote
                        "M-j" 'markdown-move-down
                        "M-k" 'markdown-move-up
                        "M-l" 'markdown-demote
                        ;; fix wrong emacs keybindings
                        "C-c C-j" 'markdown-jump
                        "C-c C-l" 'markdown-insert-list-item)
    (add-hook 'markdown-mode-hook #'my-markdown-config)))

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode))

(defun distraction-free ()
  "distraction free writing"
  (interactive)
  (git-gutter+-mode 0)
  (linum-mode 0)
  (writeroom-mode))

;; (use-package blog-admin
;;   :ensure t
;;   :defer t
;;   :init
;;   (progn
;;     (setq blog-admin-backend-path "~/github/hexo-blog/")
;;     (setq blog-admin-backend-type 'hexo)
;;     (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
;;     (setq blog-admin-backend-new-post-with-same-name-dir t) ;; create same-name directory with new post
;;     (setq blog-admin-backend-hexo-config-file "_config.yml") ;; default assumes _config.yml)
;;     )
;;   )

;; (use-package hexo :ensure t :defer t)

;;;-----------------------------------------------------------------------------
;;; org
;;;-----------------------------------------------------------------------------

(use-package org :ensure t :defer t
  :mode ("\\.org$" . org-mode)
  :config
  ;; Allow's electric-pair-mode to surround things with = and ~ in org-mode
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

  (modify-syntax-entry ?~ "(~" org-mode-syntax-table)
  (modify-syntax-entry ?= "(=" org-mode-syntax-table)
  ;; don't underline indents
  (defface org-dont-underline-indents '((t :underline nil))
	"Avoid underlining of indentation.")
  (defun org-dont-underline-indents ()
	"Remove underlining at indents."
	(add-to-list 'org-font-lock-extra-keywords '("^[[:space:]]+" 0 'org-dont-underline-indents t) 'append))
  (add-hook 'org-font-lock-set-keywords-hook #'org-dont-underline-indents 'append)

  (setq org-src-fontify-natively t ;; better looking source code
		org-return-follows-link t ;; make RET follow links
		org-hide-emphasis-markers t  ;; hide markers
		org-pretty-entities t ;; make latex look good
		org-fontify-quote-and-verse-blocks nil ;; make quotes stand out
		org-table-export-default-format "orgtbl-to-csv" ;; export for org-tables to csv
		org-ellipsis "↴"  ;; nicer elipses
		org-confirm-babel-evaluate nil  ;; evaluate src block without confirmation
		org-startup-indented t ;; start in indent mode
                                        ; org-src-preserve-indentation nil
                                        ; org-edit-src-content-indentation t
		org-imenu-depth 4
		imenu-auto-rescan t
		)

  ;; normal state shortcuts
  (general-define-key
   :states '(normal)
   :keymaps 'org-mode-map
   "RET" 'org-open-at-point     ;; Open with return in evil
   "gh" 'outline-up-heading
   "gp" 'outline-previous-heading
   "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
			'org-forward-same-level
		  'org-forward-heading-same-level)
   "gk" (if (fboundp 'org-backward-same-level)
			'org-backward-same-level 'org-backward-heading-same-level)
   "gl" 'outline-next-visible-heading
   "L" 'org-shiftright
   "H" 'org-shiftleft
   "$" 'org-end-of-line
   "^" 'org-beginning-of-line
   "<" 'org-metaleft
   ">" 'org-metaright
   "-" 'org-cycle-list-bullet)
  ;; normal & insert state shortcuts.
  (general-define-key
   :states '(normal insert)
   :keymaps 'org-mode-map
   "TAB" 'org-cycle
   "s-l" 'org-metaright
   "s-h" 'org-metaleft
   "s-k" 'org-metaup
   "s-j" 'org-metadown
   "s-L" 'org-shiftmetaright
   "s-H" 'org-shiftmetaleft
   "s-K" 'org-shiftmetaup
   "s-J" 'org-shiftmetadown
   "s-o" '(lambda () (interactive)
			(evil-org-eol-call
			 '(lambda()
				(org-insert-heading)
				(org-metaright))))
   "s-t" '(lambda () (interactive)
			(evil-org-eol-call
			 '(lambda()
				(org-insert-todo-heading nil)
				(org-metaright)))))
  ;; Use tab in insert mode
  (general-define-key
   :states '(insert)
   :keymaps 'org-mode-map
   "\t" nil)

  (use-package org-bullets :ensure t :defer t)
  (use-package htmlize :ensure t :defer t)

  (use-package toc-org :ensure t :defer t
	:init (setq toc-org-max-depth 10))

  (use-package ox-pandoc :ensure t :defer t
	:commands ox-pandoc
	;; (require 'ox-pandoc)
	:config
	;; default options for all output formats
	;; (setq org-pandoc-command (expand-file-name "~/.local/bin/pandoc"))
	(setq org-pandoc-options '((standalone . t)))
	;; cancel above settings only for 'docx' format
	(setq org-pandoc-options-for-docx '((standalone . nil)))
	;; special settings for beamer-pdf and latex-pdf exporters
	(setq org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
	(setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex"))))

  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'org-mode-hook 'toc-org-enable)
  (add-hook 'org-mode-hook
			'(lambda()
			   (turn-on-auto-fill)
			   (set-fill-column 78)
			   (flyspell-mode 1)
			   (hl-todo-mode)
			   (imenu-add-to-menubar "Imenu")
			   (git-gutter+-mode 0)
			   (org-eldoc-load))))

(setq org-file-apps
      '(("\\.docx\\'" . default)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)
        (auto-mode . emacs)))

(use-package ox-reveal :ensure t :disabled t :after org
  :config
  (setq org-reveal-root (concat "file://" (getenv "HOME") "/bin/reveal.js")
        org-reveal-theme "moon"
        org-reveal-default-frag-style "roll-in"
        org-reveal-hlevel 2))

(with-eval-after-load 'org
  (defvar-local rasmus/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun rasmus/org-prettify-src--update ()
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
          found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args rasmus/ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))
        (setq prettify-symbols-alist
              (cl-set-difference prettify-symbols-alist
                                 (cl-set-difference
                                  (cl-remove-if-not
                                   (lambda (elm)
                                     (eq (cdr elm) rasmus/ob-header-symbol))
                                   prettify-symbols-alist)
                                  found :test #'equal)))
        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun rasmus/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.

  `prettify-symbols-mode' is used because it has uncollpasing. It's
  may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block (save-excursion
                           (beginning-of-line)
                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and rasmus/org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq rasmus/org-at-src-begin -1))
        (rasmus/org-prettify-src--update))
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq rasmus/org-at-src-begin at-src-block)))

  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?╦) ;; ➤ 🖝 ➟ ➤ ✎ ✎
                               ("#+end_src"   . ?╩) ;; □
                               ("#+header:" . ,rasmus/ob-header-symbol)
                               ("#+begin_comment" . ?✎)
                               ("#+end_comment" . ?✎)
                               ("#+begin_notes" . ?➤)
                               ("#+end_notes" . ?➤)
                               ("#+begin_quote" . ?»)
                               ("#+end_quote" . ?«)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
  (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))

;;;-----------------------------------------------------------------------------
;;; latex config
;;;-----------------------------------------------------------------------------
(use-package auctex :ensure t :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (use-package company-auctex :ensure t :defer t)
  (setq
   TeX-auto-save t
   TeX-parse-self t
   TeX-save-query nil
   TeX-PDF-mode t
   ;; Synctex support
   TeX-source-correlate-start-server nil
   ;; Don't insert line-break at inline math
   LaTeX-fill-break-at-separators nil
   )

  (setq-default TeX-master nil)
  (setq-default TeX-engine 'xelatex)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)

  (push '(company-auctex-bibs
		  company-auctex-labels
		  company-auctex-macros
		  company-auctex-symbols
		  company-auctex-environments)
		company-backends-LaTeX-mode))

(use-package reftex :ensure t :defer t :commands turn-on-reftex
  :config (setq reftex-plug-into-AUCTeX t))

(use-package bibtex :ensure t :defer t
  :mode ("\\.bib$" . bibtex-mode)
  :config
  (setq bibtex-align-at-equal-sign t)
  (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120))))

;; Auto-fill for LaTeX
(defun schnouki/latex-auto-fill ()
  "Turn on auto-fill for LaTeX mode."
  (turn-on-auto-fill)
  (set-fill-column 80)
  (setq default-justification 'left))
(add-hook 'LaTeX-mode-hook #'schnouki/latex-auto-fill)

;; Compilation command
(add-hook 'LaTeX-mode-hook
		  (lambda ()
			(setq compile-command "latexmk -pdflatex=xelatex -f -pdf %f")))

(use-package latex-preview-pane :ensure t :defer 30 :after auctex
  :bind (:map
		 latex-preview-pane-mode
		 ("C-c u p" . latex-preview-pane-update)
		 ("C-c u P" . latex-preview-update))
  :init
  (latex-preview-pane-enable)
  (setq pdf-latex-command "xelatex"))

(eval-after-load 'doc-view-mode
  '(lambda ()
	 (setq doc-view-resolution 300)))

(provide 'config-writing)
;;; config-tex.el ends here
