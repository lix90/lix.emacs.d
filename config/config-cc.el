;;; for c and c++
(defun init-cc-mode ()
  (use-package cc-mode
    :ensure t
    :defer t
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1))
    )

  (use-package disaster
    :ensure t
    :defer t
    :commands (disaster)
    :config
    (progn
      (define-key c-mode-base-map (kbd "C-c d") 'disaster)))

  (use-package clang-format
    :ensure t
    :defer t
    :config
    (setq clang-format-executable "clang-format"))

  )

(init-cc-mode)

(provide 'config-cc)
;;; config-cc.el ends here
