;;; Common lisp
(use-package slime :ensure t :defer t
  :commands (slime)
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-contribs '(slime-fancy)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-elisp)))

(provide 'config-lisp)
