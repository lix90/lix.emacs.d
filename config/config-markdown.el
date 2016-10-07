

;; -----------------------------------
;; markdown
;; -----------------------------------
(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.text\\'" . markdown-mode))
  :config (progn
            (add-hook 'markdown-mode-hook 'visual-line-mode)
            (add-hook 'markdown-mode-hook 'company-mode)
            (add-hook 'markdown-mode-hook 'smartparens-mode)
            (use-package markdown-toc
              :ensure t)
            ))

(provide 'config-markdown)

;;; config-markdown.el ends here
