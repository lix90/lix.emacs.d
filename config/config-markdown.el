;;; config-markdown.el --- Configuration for writing markdown:

;;; Commentary:

;;; Code:
(use-package markdown-mode :ensure t :defer t
  :mode (("\\.markdown$" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.md$"       . markdown-mode)
         ("\\.txt$" . markdown-mode)
         ("\\.Rmd\\'" . markdown-mode))
  :bind (("C-c t m" . markdown-toggle-markup-hiding))
  :init
  (setq markdown-command "pandoc")
  (setq markdown-header-scaling t)
  :config
  (use-package markdown-mode+ :ensure t :defer t)
  ;;(add-hook 'markdown-mode-hook #'yas-minor-mode)
  (setq ;;markdown-command "pandoc"
   markdown-enable-math t
   markdown-footnote-location "end"
   markdown-nested-imenu-heading-index t)
  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-delimiter-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))) t)
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))) t)
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.3))) t)
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :underline t :height 1.2))) t)
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :underline t :height 1.1))) t)
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :underline t))) t)
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :underline t))) t))
(put 'set-goal-column 'disabled nil)

(use-package writeroom-mode :ensure t :defer t
  :commands (writeroom-mode)
  :bind (("C-c t w" . writeroom-mode)))

(provide 'config-markdown)
;;; config-markdown.el ends here
