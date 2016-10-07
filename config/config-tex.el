
;;; latex config
(use-package auctex
  :ensure t
  :defer t
  :mode
  ("\\.latex$" . TeX-latex-mode)
  ("\\.tex$" . TeX-latex-mode)
  :init
  (progn
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-syntactic-comment t
          ;; Synctex support
          TeX-source-correlate-start-server nil
          ;; Don't insert line-break at inline math
          LaTeX-fill-break-at-separators nil)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
    (setq-default TeX-engine 'xelatex)
    ))

(use-package company-auctex
  :ensure t
  :defer t)

(use-package latex-preview-pane
  :ensure t
  :defer t
  :config
  (latex-preview-pane-enable))

(eval-after-load 'latex-mode
  '(progn
     (add-hook 'LaTeX-mode-hook 'smartparens-mode)
     (push 'company-auctex-labels company-backends-LaTeX-mode)
     (push 'company-auctex-bibs company-backends-LaTeX-mode)
     (push '(company-auctex-macros
             company-auctex-symbols
             company-auctex-environments) company-backends-LaTeX-mode)
     ;; (add-hook 'plain-TeX-mode-hook
     ;;           '(lambda ()
     ;;              (set (make-variable-buffer-local 'TeX-electric-math) (cons "$" "$"))))
     ;; (add-hook 'LaTeX-mode-hook
     ;;           '(lambda ()
     ;;              (set (make-variable-buffer-local 'TeX-electric-math) (cons "\\(" "\\)"))))
     )
  )



(provide 'config-tex)
;;; config-tex.el ends here
