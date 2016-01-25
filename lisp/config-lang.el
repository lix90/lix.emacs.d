(use-package ess-site
  :ensure ess
  :commands R
  :init (progn
          ;; TODO: why doesn't use-package require it for us?
          (require 'ess-site)
          (setq ess-eval-visibly-p nil
                ess-use-tracebug t
                ess-use-auto-complete t
                ess-help-own-frame 'one
                ess-ask-for-ess-directory nil)
          (setq-default ess-dialect "R")
          (ess-toggle-underscore t)))

;; Octave
(use-package octave-mode
  :mode "\\.m$")

;; matlab
(use-package matlab-mode
  :ensure t
  :defer t
  :init (setq matlab-shell-command
              (or
               (executable-find "matlab")
               (executable-find "/usr/local/bin/matlab")
               (executable-find "/Applications/Matlab.app/bin/matlab")))
  :config (setq matlab-indent-function-body t)
  :commands (matlab-mode matlab-shell))

(defalias 'run-matlab 'matlab-shell)

;; markdown
(use-package markdown-mode
  :ensure t
  :commands markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
(use-package markdown-toc
  :ensure t
  )

;; python-mode
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :config (progn
            (use-package elpy
              :ensure t
              :init (setq elpy-rpc-backend "jedi")
              :config (progn
                        (elpy-enable)
                        (elpy-use-ipython)))))

(use-package company-jedi
  :ensure t
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

(provide 'config-lang)
