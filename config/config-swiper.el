;;; swiper, flx, counsel, ivy
(use-package flx :ensure t)
(use-package swiper
  :diminish ivy-mode
  :commands (swiper ivy-resume counsel-M-x counsel-find-file)
  :ensure counsel
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c i" . ivy-immediate-done)
         )
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-use-virtual-buffers t))

(provide 'config-swiper)

;;; config-swiper.el ends here
