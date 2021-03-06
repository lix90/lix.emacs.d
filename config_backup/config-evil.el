;;; Evil mode settings
(use-package evil :ensure t :defer t
  :init (evil-mode 1)
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

    (setq evil-normal-state-tag   (propertize "N" )
          evil-emacs-state-tag    (propertize "E" )
          evil-insert-state-tag   (propertize "I" )
          evil-motion-state-tag   (propertize "M" )
          evil-visual-state-tag   (propertize "V" )
          evil-operator-state-tag (propertize "O" ))
    
    (setq evil-search-module 'evil-search
          evil-magic 'very-magic
          evil-emacs-state-cursor `(,dotemacs-evil/emacs-cursor box)
          evil-normal-state-cursor '("DarkGoldenrod2" box)
          evil-visual-state-cursor '("gray" box)
          evil-insert-state-cursor '("chartreuse3" (bar . 2))
          evil-replace-state-cursor '("red" hbar)
          evil-operator-state-cursor '("red" hollow))
    
    ;; (run-with-idle-timer 30 t 'evil-normal-state)

    (general-define-key
     :states '(normal visual)
     "j" 'evil-next-visual-line
     "k" 'evil-previous-visual-line)

    (use-package evil-escape :ensure t 
      :init
      (evil-escape-mode)
      ;; use "fd" for escape
      (setq-default evil-escape-key-sequence "fd"
                    evil-escape-delay 0.2))

    (use-package evil-terminal-cursor-changer :ensure t :defer t
      :if (display-graphic-p)
      :config
      ;;(require 'evil-terminal-cursor-changer)
      (setq evil-visual-state-cursor '("red" box)); █
      (setq evil-insert-state-cursor '("green" bar)); ⎸
      (setq evil-emacs-state-cursor '("blue" hbar)); _
      )

    (use-package evil-surround :ensure t
      :commands (evil-surround-region evil-surround-change)
      :config
      (global-evil-surround-mode 1)
      (general-define-key
       :states '(visual)
       "s" 'evil-surround-region
       "S" 'evil-substitute))

    (use-package evil-embrace :ensure t :defer t
      :init
      (progn
        (evil-embrace-enable-evil-surround-integration)
        (add-hook 'org-mode-hook 'embrace-org-mode-hook)))

    (use-package evil-commentary :ensure t :defer t
      :commands (evil-commentary evil-commentary-line)
      :diminish evil-commentary-mode)

    (use-package evil-indent-textobject :ensure t :defer t)
    (use-package evil-indent-plus :ensure t :defer t
      :init (evil-indent-plus-default-bindings))
    
    ))

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))


;; (defun my-evil-terminal-cursor-change ()
;;   (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
;;     (add-hook 'evil-insert-state-entry-hook
;;               (lambda ()
;;                 (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
;;     (add-hook 'evil-insert-state-exit-hook
;;               (lambda ()
;;                 (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
;;   (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
;;     (add-hook 'evil-insert-state-entry-hook
;;               (lambda ()
;;                 (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
;;     (add-hook 'evil-insert-state-exit-hook
;;               (lambda ()
;;                 (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))
;;
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame) (my-evil-terminal-cursor-change)))
;; (my-evil-terminal-cursor-change)

;; to save the buffer when we exit the insert mode
(defun my-save-if-bufferfilename ()
  (if (buffer-file-name)
      (progn
        (save-buffer))
    (message "no file is associated to this buffer: do nothing")))
(add-hook 'evil-insert-state-exit-hook #'my-save-if-bufferfilename)

;; Enter an emacs mode in a given state
(loop for (mode . state)
      in '((inferior-emacs-lisp-mode . emacs)
           (comint-mode . emacs)
           (shell-mode . emacs)
           (git-commit-mode . insert)
           (git-rebase-mode . emacs)
           (term-mode . emacs)
           (help-mode . emacs)
           (magit-branch-manager-mode . emacs)
           (dired-mode . emacs)
           (inferior-ess-mode . emacs)
           (inferior-python-mode . emacs)
           (matlab-shell-mode . emacs)
           (inferior-js-mode . emacs)
           (eshell-mode . emacs)
		   (neotree-mode . emacs)
           (elfeed-search-mode . emacs)
           (elfeed-show-mode . emacs)
           )
      do (evil-set-initial-state mode state))


(provide 'config-evil)
;;; Config-evil.el ends here
