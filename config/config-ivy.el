;;; stolen from: https://github.com/marktran/emacs.d
(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)

  :config
  (use-package counsel
    :ensure t)
  (use-package flx :ensure t)
  (use-package swiper
    :ensure t
    :bind (("<f5>" . swiper)))

  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t

        ivy-ignore-buffers `("^\\*alchemist-server\\*"
                             "^\\*alchemist test report\\*"
                             "^\\*Compile-Log\\*"
                             "^\\*Completions\\*"
                             "^\\*Help\\*"
                             "^\\*Messages\\*"
                             "^\\*Warnings\\*"
                             "^\\*eshell"
                             "^\\*magit"
                             "^\\*scratch\\*"
                             "^\\*rspec-compilation\\*"
                             (lambda (name)
                               (save-excursion
                                 (equal major-mode 'dired-mode))))

        ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(provide 'config-ivy)
;;; config-ivy.el ends here
