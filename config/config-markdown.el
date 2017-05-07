;;; markdown
(use-package markdown-mode :ensure t :defer t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$"       . markdown-mode)
         ("\\.txt$" . markdown-mode))
  :config
  ;; markdown hooks
  (add-hook 'markdown-mode-hook
            '(lambda()
               ;; (linum-mode)
               ;; (mmm-mode +1)
               ;;(centered-cursor-mode)
               ;;(flyspell-mode 1)
               ;;(pandoc-mode)
               (yas-minor-mode t)
               (hl-todo-mode t)))
  (setq markdown-command "pandoc"
        markdown-enable-math t
        markdown-footnote-location "end"
        markdown-nested-imenu-heading-index t)
  ;;(add-hook 'markdown-mode-hook #'my-markdown-config)
  )

(use-package writeroom-mode :ensure t
  :commands (writeroom-mode)
  :config
  (defun distraction-free ()
    "distraction free writing"
    (interactive)
    (writeroom-mode)))

(provide 'config-markdown)
