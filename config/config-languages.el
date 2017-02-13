;;; Common lisp
(use-package slime :ensure t :defer t
  :commands (slime)
  :config
  (setq inferior-lisp-program (executable-find "sbcl"))
  (setq slime-contribs '(slime-fancy)))

;;; for c and c++
(use-package cc-mode :ensure t :defer t
  :config
  (require 'compile)
  (c-toggle-auto-newline 1))

(use-package disaster :ensure t :defer t :commands (disaster)
  :config (define-key c-mode-base-map (kbd "C-c d") 'disaster))

(use-package clang-format :ensure t :defer t
  :config (setq clang-format-executable "clang-format"))

;;; for java
(use-package eclim :ensure t :defer t :diminish eclim-mode
  :init (add-hook 'java-mode-hook 'eclim-mode)
  :config
  (setq help-at-pt-display-when-idle t
		help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)
  (use-package company-emacs-eclim :ensure t :defer t
	:config (company-emacs-eclim-setup)))

;;; for ruby
(use-package enh-ruby-mode :ensure t :defer t
  :interpreter "ruby"
  :mode (("\\.rake\\'" . enh-ruby-mode)
         ("Rakefile\\'" . enh-ruby-mode)
         ("\\.gemspec\\'" . enh-ruby-mode)
         ("\\.ru\\'" . enh-ruby-mode)
         ("\\.rb\\'" . enh-ruby-mode)
         ("Gemfile\\'" . enh-ruby-mode)
         ("\\.builder\\'" . enh-ruby-mode)
         ("Guardfile\\'" . enh-ruby-mode)
         ("\\.thor\\'" . enh-ruby-mode)
         ("Thorfile\\'" . enh-ruby-mode)
         ("Vagrantfile\\'" . enh-ruby-mode)
         ("\\.jbuilder\\'" . enh-ruby-mode)
         )
  :config
  (use-package robe :ensure t :defer t)
  (use-package inf-ruby :ensure t :defer t)
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'enh-ruby-mode-hook 'robe-start)
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-to-list 'company-backends 'company-robe)
  (require 'smartparens-ruby)
  )

;;; matlab
(use-package matlab-mode :ensure t :defer t
  :mode ("\\.m\\'" . matlab-mode)
  :commands (matlab-mode matlab-shell))
(eval-after-load 'matlab
  '(progn
     (setq matlab-indent-function-body t)
     (setq matlab-shell-command-switches '("-nosplash -nodesktop"))
     (setq matlab-shell-command
           (or
            (executable-find "matlab")
            (executable-find "/usr/local/bin/matlab")
            (executable-find "/Applications/MATLAB_R2014b.app/bin/matlab")))
     ;;(add-hook 'matlab-mode-hook 'company-mode)
     ;;(add-hook 'matlab-mode-hook 'smartparens-mode)
     ;; (add-to-list 'company-backends 'company-matlab-shell)
     (evil-set-initial-state 'matlab-shell-mode 'emacs)))

;;; sql
(use-package sql :ensure t :defer t
  :mode (("\.sql$" . sql-mode)
         ("\.sqltmpl$" . sql-mode))
  :config
  (use-package sql-indent :ensure t
	:config
	(setq sql-indent-offset 2))
  (use-package sqlup-mode :ensure t
	:config
	(add-hook 'sql-mode-hook 'sqlup-mode)
	(add-hook 'sql-interactive-mode-hook 'sqlup-mode)))


(provide 'config-languages)
;;; config-languages.el ends here
