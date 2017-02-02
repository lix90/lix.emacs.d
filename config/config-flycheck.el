;;; flycheck --- flycheck mode configuration
;;; Commentary:
;;; code:

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(json-jsonlist)))

(use-package flycheck
  :defer t
  :init
  (progn
    (setq flycheck-standard-error-navigation nil
          flycheck-global-modes nil)

    ;; Custom fringe indicator
    (when (and (fboundp 'define-fringe-bitmap)
               (not syntax-checking-use-original-bitmaps))
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (let ((bitmap (if syntax-checking-use-original-bitmaps
                      'flycheck-fringe-bitmap-double-arrow
                    'my-flycheck-fringe-indicator)))

      (flycheck-define-error-level 'error
                                   :severity 2
                                   :overlay-category 'flycheck-error-overlay
                                   :fringe-bitmap bitmap
                                   :fringe-face 'flycheck-fringe-error)
      (flycheck-define-error-level 'warning
                                   :severity 1
                                   :overlay-category 'flycheck-warning-overlay
                                   :fringe-bitmap bitmap
                                   :fringe-face 'flycheck-fringe-warning)
      (flycheck-define-error-level 'info
                                   :severity 0
                                   :overlay-category 'flycheck-info-overlay
                                   :fringe-bitmap bitmap
                                   :fringe-face 'flycheck-fringe-info))
    ;; copied from spacemacs
    ;; toggle flycheck window
    (defun lix--toggle-flycheck-error-list ()
      "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
      (interactive)
      (-if-let (window (flycheck-get-error-list-window))
          (quit-window nil window)
        (flycheck-list-errors)))

    (defun lix--goto-flycheck-error-list ()
      "Open and go to the error list buffer."
      (interactive)
      (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
        (flycheck-list-errors)
        (switch-to-buffer-other-window flycheck-error-list-buffer)))

    (evilified-state-evilify-map flycheck-error-list-mode-map
                                 :mode flycheck-error-list-mode
                                 :bindings
                                 "RET" 'flycheck-error-list-goto-error
                                 "j" 'flycheck-error-list-next-error
                                 "k" 'flycheck-error-list-previous-error)))

(use-package flycheck-pos-tip
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(provide 'config-flycheck)
;;; config-flycheck.el ends here
