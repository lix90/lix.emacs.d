;;;
(use-package evil
  :ensure t
  :config
  (progn
    ;; Cursor shape and color
    (defcustom dotemacs-evil/emacs-cursor
      "red"
      "The color of the cursor when in Emacs state."
      :type 'color
      :group 'dotemacs-evil)

    (defcustom dotemacs-evil/emacs-insert-mode
      nil
      "If non-nil, insert mode will act as Emacs state."
      :type 'boolean
      :group 'dotemacs-evil)

    (setq evil-search-module 'evil-search)
    (setq evil-magic 'very-magic)
    (setq evil-emacs-state-cursor `(,dotemacs-evil/emacs-cursor box))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
    (setq evil-visual-state-cursor '("gray" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
    (setq evil-replace-state-cursor '("red" hbar))
    (setq evil-operator-state-cursor '("red" hollow))
    (run-with-idle-timer 20 t 'evil-normal-state)

    (general-define-key
     :states '(normal visual)
     "j" 'evil-next-visual-line
     "k" 'evil-previous-visual-line)

    (use-package evil-escape
      :ensure t
      :diminish ""
      :init
      (evil-escape-mode)
      ;; use "fd" for escape
      (setq-default evil-escape-key-sequence "fd"
                    evil-escape-delay 0.2))

    ;; (use-package evil-indent-textobject :ensure t)

    (use-package evil-numbers
      :ensure t
      :defer t
      :init
      (general-define-key
       :states '(normal visual insert emacs)
       "H-s" 'evil-numbers/inc-at-pt
       "H-a" 'evil-numbers/dec-at-pt))

    (use-package evil-terminal-cursor-changer
      :ensure t
      :if (display-graphic-p)
      :disabled nil
      :defer t
      :config
      ;;(require 'evil-terminal-cursor-changer)
      (progn
        (setq evil-visual-state-cursor '("red" box)); █
        (setq evil-insert-state-cursor '("green" bar)); ⎸
        (setq evil-emacs-state-cursor '("blue" hbar)); _
        ))

    (use-package evil-surround
      :ensure t
      ;; :commands (evil-surround-region evil-surround-change)
      :config
      (progn
        (global-evil-surround-mode 1)
        (general-define-key
         :states '(visual)
         "s" 'evil-surround-region
         "S" 'evil-substitute)))

    (use-package evil-commentary
      :ensure t
      :commands (evil-commentary evil-commentary-line)
      :diminish evil-commentary-mode
      :config
      (evil-commentary-mode))
    )
  :init
  (evil-mode 1))

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
(my-evil-terminal-cursor-change)

(provide 'config-evil)
;;; Config-evil.el ends here
