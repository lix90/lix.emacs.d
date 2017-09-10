;;; config-org.el --- Configuration for org-mode:

;;; Commentary:

;;; Code:

(use-package org :ensure t :defer t
  :diminish org-indent-mode
  :mode ("\\.org$" . org-mode)
  :config
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  ;; (modify-syntax-entry ?~ "(~" org-mode-syntax-table)
  ;; (modify-syntax-entry ?= "(=" org-mode-syntax-table)
  ;; don't underline indents
  (defface org-dont-underline-indents '((t :underline nil))
    "Avoid underlining of indentation.")
  (defun org-dont-underline-indents ()
    "Remove underlining at indents."
    (add-to-list 'org-font-lock-extra-keywords '("^[[:space:]]+" 0 'org-dont-underline-indents t) 'append))
  (add-hook 'org-font-lock-set-keywords-hook #'org-dont-underline-indents 'append)

  (setq
   ;; better looking source code
   org-src-fontify-natively t
   ;; make RET follow links
   org-return-follows-link t
   ;; hide markers
   org-hide-emphasis-markers t
   ;; make latex look good
   org-pretty-entities t
   ;; make quotes stand out
   org-fontify-quote-and-verse-blocks nil
   ;; export for org-tables to csv
   org-table-export-default-format "orgtbl-to-csv" 
   ;; nicer elipses
   org-ellipsis "↴"
   ;; evaluate src block without confirmation
   org-confirm-babel-evaluate nil
   ;; start in indent mode
   org-startup-indented t 
   ;; org-src-preserve-indentation nil
   ;; org-edit-src-content-indentation t
   org-imenu-depth 4
   imenu-auto-rescan t)
  
  (add-hook 'org-mode-hook
            '(lambda()
               ;;(turn-on-auto-fill)
               ;;(set-fill-column 78)
               ;;(flyspell-mode 1)
               ;;(hl-todo-mode)
               (imenu-add-to-menubar "Imenu")
               ;;(git-gutter+-mode 0)
               ;;(org-eldoc-load)
               (org-bullets-mode t)
               (toc-org-enable)))
  
  (setq org-file-apps
        '(("\\.docx\\'" . default)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . default)
          (auto-mode . emacs))))

(use-package org-bullets :ensure t :defer t)

(use-package htmlize :ensure t :defer t)

(use-package toc-org :ensure t :defer t
  :init (setq toc-org-max-depth 10))

(use-package ox-pandoc :ensure t :defer t
  :commands ox-pandoc 
  :config
  ;; default options for all output formats
  ;; (setq org-pandoc-command (expand-file-name "~/.local/bin/pandoc"))
  (setq org-pandoc-options '((standalone . t)))
  ;; cancel above settings only for 'docx' format
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((latex-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((latex-engine . "xelatex"))))

(use-package ox-reveal :ensure t :disabled t :after org
  :config
  (setq org-reveal-root (concat "file://" (getenv "HOME") "/bin/reveal.js")
        org-reveal-theme "moon"
        org-reveal-default-frag-style "roll-in"
        org-reveal-hlevel 2))


;; (with-eval-after-load 'org
;;   (defvar-local rasmus/org-at-src-begin -1
;;     "Variable that holds whether last position was a ")

;;   (defvar rasmus/ob-header-symbol ?☰
;;     "Symbol used for babel headers")

;;   (defun rasmus/org-prettify-src--update ()
;;     (let ((case-fold-search t)
;;           (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
;;           found)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward re nil t)
;;           (goto-char (match-end 0))
;;           (let ((args (org-trim
;;                        (buffer-substring-no-properties (point)
;;                                                        (line-end-position)))))
;;             (when (org-string-nw-p args)
;;               (let ((new-cell (cons args rasmus/ob-header-symbol)))
;;                 (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
;;                 (cl-pushnew new-cell found :test #'equal)))))
;;         (setq prettify-symbols-alist
;;               (cl-set-difference prettify-symbols-alist
;;                                  (cl-set-difference
;;                                   (cl-remove-if-not
;;                                    (lambda (elm)
;;                                      (eq (cdr elm) rasmus/ob-header-symbol))
;;                                    prettify-symbols-alist)
;;                                   found :test #'equal)))
;;         ;; Clean up old font-lock-keywords.
;;         (font-lock-remove-keywords nil prettify-symbols--keywords)
;;         (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
;;         (font-lock-add-keywords nil prettify-symbols--keywords)
;;         (while (re-search-forward re nil t)
;;           (font-lock-flush (line-beginning-position) (line-end-position))))))

;;   (defun rasmus/org-prettify-src ()
;;     "Hide src options via `prettify-symbols-mode'.

;;   `prettify-symbols-mode' is used because it has uncollpasing. It's
;;   may not be efficient."
;;     (let* ((case-fold-search t)
;;            (at-src-block (save-excursion
;;                            (beginning-of-line)
;;                            (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
;;       ;; Test if we moved out of a block.
;;       (when (or (and rasmus/org-at-src-begin
;;                      (not at-src-block))
;;                 ;; File was just opened.
;;                 (eq rasmus/org-at-src-begin -1))
;;         (rasmus/org-prettify-src--update))
;;       ;; Remove composition if at line; doesn't work properly.
;;       ;; (when at-src-block
;;       ;;   (with-silent-modifications
;;       ;;     (remove-text-properties (match-end 0)
;;       ;;                             (1+ (line-end-position))
;;       ;;                             '(composition))))
;;       (setq rasmus/org-at-src-begin at-src-block)))

;;   (defun rasmus/org-prettify-symbols ()
;;     (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
;;           (cl-reduce 'append
;;                      (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
;;                              `(("#+begin_src" . ?╦) ;; ➤ 🖝 ➟ ➤ ✎ ✎
;;                                ("#+end_src"   . ?╩) ;; □
;;                                ("#+header:" . ,rasmus/ob-header-symbol)
;;                                ("#+begin_comment" . ?✎)
;;                                ("#+end_comment" . ?✎)
;;                                ("#+begin_notes" . ?➤)
;;                                ("#+end_notes" . ?➤)
;;                                ("#+begin_quote" . ?»)
;;                                ("#+end_quote" . ?«)))))
;;     (turn-on-prettify-symbols-mode)
;;     (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
;;   (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))

(provide 'config-org)
;;; config-org.el ends here
