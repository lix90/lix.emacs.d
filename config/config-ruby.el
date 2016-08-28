;; packages for ruby
;; bundler: Interact with Bundler from Emacs
;; robe: Code navigation, documentation lookup and completion for Ruby
;; rbenv: use rbenv to manage your Ruby versions within Emacs
(defun init-ruby ()

  (message "Initialize ruby module")

  (use-package enh-ruby-mode
    :interpreter "ruby"
    :defer 2
    :mode (("Rakefile\\'" . ruby-mode)
           (".rake\\'" . ruby-mode)
           ("\\.rb\\'" . ruby-mode)
           ("\\.builder\\'" . ruby-mode)
           ("\\.ru\\'" . ruby-mode)
           ("\\.gemspec\\'" . ruby-mode)
           ("Gemfile\\'" . ruby-mode))
    :config
    (progn

      (message "enh ruby-mode")

      (use-package robe
        :ensure t)

      (use-package rvm
        :ensure t
        :config
        (rvm-use-default))

      (use-package rbenv
        :ensure t
        :config   (setq rbenv-installation-dir "~/rbenv")
        )

      (add-hook 'enh-ruby-mode-hook 'robe-mode)
      (add-hook 'enh-ruby-mode-hook 'robe-start)
      (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
      (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
      (add-to-list 'company-backends 'company-robe)

      )
    )
  )

  (init-ruby)


  (provide 'config-ruby)
;;; config-ruby.el ends here
