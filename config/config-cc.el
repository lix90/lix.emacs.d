;;; for c and c++
(use-package cc-mode :ensure t :defer t
  :config
  (require 'compile)
  (c-toggle-auto-newline 1))

(use-package disaster :ensure t :defer t :commands (disaster)
  :config (define-key c-mode-base-map (kbd "C-c d") 'disaster))

(use-package clang-format :ensure t :defer t
  :config (setq clang-format-executable "clang-format"))

(provide 'config-cc)
