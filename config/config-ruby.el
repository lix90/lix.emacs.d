;; packages for ruby
;; bundler: Interact with Bundler from Emacs
;; robe: Code navigation, documentation lookup and completion for Ruby
;; rbenv: use rbenv to manage your Ruby versions within Emacs


(use-package enh-ruby-mode
  :interpreter "ruby"
  :mode (
         ("\\.rake\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("\\.ru\\'" . ruby-mode)
         ("\\.rb\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("\\.builder\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)
         ("\\.thor\\'" . ruby-mode)
         ("Thorfile\\'" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.jbuilder\\'" . ruby-mode)
         ))

(use-package robe
  :ensure t)
;; (use-package inf-ruby :ensure t :defer t)
;; (use-package rvm
;;   :ensure t
;;   :config
;;   (rvm-use-default))
;; (use-package rbenv
;;   :ensure t
;;   :config
;;   (setq rbenv-installation-dir "~/rbenv")
;;   )

(eval-after-load 'enh-ruby-mode
  '(progn
     (add-hook 'enh-ruby-mode-hook 'robe-mode)
     (add-hook 'enh-ruby-mode-hook 'robe-start)
     (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
     (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
     (add-to-list 'company-backends 'company-robe)
     (require 'smartparens-ruby)
     ))

(provide 'config-ruby)
;;; config-ruby.el ends here
