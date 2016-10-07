;;; swiper, flx, counsel, ivy
(use-package flx :ensure t)
(use-package swiper
  :ensure counsel
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         )
  :init
  (progn
    (ivy-mode 1)
    (setq ivy-re-builders-alist
          '((ivy-switch-buffer . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
    )
  :config
  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t))

(provide 'config-swiper)

;;; config-swiper.el ends here
