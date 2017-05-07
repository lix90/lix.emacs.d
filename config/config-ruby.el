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

(provide 'config-ruby)
