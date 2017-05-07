;;;-----------------------------------------------------------------------------
;;; latex config
;;;-----------------------------------------------------------------------------
(use-package auctex :ensure t :defer t
  :mode ("\\.tex$" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-PDF-mode t
        TeX-source-correlate-start-server nil
        LaTeX-fill-break-at-separators nil)
  (setq-default TeX-master nil)
  (setq-default TeX-engine 'xelatex)
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (LaTeX-preview-setup)
              (turn-on-reftex)
              (smartparens-mode t)
              (LaTeX-math-mode t)
              (TeX-source-correlate-mode t)
              (push '(company-auctex-bibs
                      company-auctex-labels
                      company-auctex-macros
                      company-auctex-symbols
                      company-auctex-environments)
                    company-backends-LaTeX-mode))))

(use-package company-auctex :ensure t :defer t)

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

(use-package latex-preview-pane :ensure t :defer t :after auctex
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (latex-preview-pane-enable)
              (setq pdf-latex-command "xelatex")
              (local-set-key (kbd "C-c u p") 'latex-preview-pane-update)
              (local-set-key (kbd "C-c u P") 'latex-preview-update))))

(provide 'config-latex)
