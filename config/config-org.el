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
  ;; 静止下划线缩进
  (defface org-dont-underline-indents '((t :underline nil))
    "Avoid underlining of indentation.")
  (defun org-dont-underline-indents ()
    "Remove underlining at indents."
    (add-to-list 'org-font-lock-extra-keywords '("^[[:space:]]+" 0 'org-dont-underline-indents t) 'append))
  (add-hook 'org-font-lock-set-keywords-hook #'org-dont-underline-indents 'append)

  (setq org-src-fontify-natively t ;; 美化源代码
        org-return-follows-link t ;; make RET follow links
        org-hide-emphasis-markers t ;; hide markers
        org-pretty-entities t ;; make latex look good
        org-fontify-quote-and-verse-blocks nil ;; make quotes stand out
        org-table-export-default-format "orgtbl-to-csv" ;; export for org-tables to csv        
        org-ellipsis "↴" ;; nicer elipses        
        org-confirm-babel-evaluate nil ;; evaluate src block without confirmation
        org-startup-indented t  ;; start in indent mode
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

(setq org-agenda-files '("~/self/org/journal.org"))

(setq org-capture-templates
      '("j" "Journal"
        entry (file+datetree "~/self/org/journal.org")
        "* %U - %^{heading} %^g\n %?\n"
        ))


(bind-keys :prefix-map org-keybindings
           :prefix "C-c o"
           :prefix-docstring "Org Mode Keybindings"
           ("c" . org-capture)
           ("a" . org-agenda))

(provide 'config-org)
;;; config-org.el ends here
