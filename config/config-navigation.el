
;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c C-j w" . ace-jump-word-mode)
         ("C-c C-j c" . ace-jump-char-mode)
         ("C-c C-j l" . ace-jump-line-mode)))

(provide 'config-navigation)
;;; config-navigation.el ends here
