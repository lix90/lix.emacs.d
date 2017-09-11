;;; config-markdown.el --- Configuration for writing markdown:

;;; Commentary:

;;; Code:
(use-package markdown-mode :ensure t :defer t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$"       . markdown-mode)
         ("\\.txt$" . markdown-mode))
  :config
  (use-package markdown-mode+ :ensure t :defer t)
  (add-hook 'markdown-mode-hook #'yas-minor-mode)
  (setq markdown-command "pandoc"
        markdown-enable-math t
        markdown-footnote-location "end"
        markdown-nested-imenu-heading-index t))

;;; 在markdown模式下，C-c ' 通过另外一个buffer来编辑源代码块
(use-package edit-indirect :ensure t :defer t)

(use-package writeroom-mode :ensure t :defer t
  :commands (writeroom-mode)
  :config
  (defun distraction-free ()
    "distraction free writing"
    (interactive)
    (writeroom-mode)))

(provide 'config-markdown)
;;; config-markdown.el ends here
