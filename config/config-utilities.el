;;; upgrade-packages
(use-package popup :ensure t :defer t
  :config
  ;; Change popup tip face
  ;; TODO make popup tip scrollable
  (set-face-attribute 'popup-tip-face nil
                      :foreground "#aaaaaa"
                      :background "#282c32")
  )

(use-package popwin :ensure t
  :config (popwin-mode 1)
  (setq popwin:close-popup-window-timer-interval 0.1)
  (setq popwin:close-popup-window-timer nil)
  (defun popwin:flycheck-errors ()
    (interactive)
    (when (get-buffer "*Flycheck errors*") (popwin:popup-buffer "*Flycheck errors*")))
  (defun popwin:compilation ()
    (interactive)
    (when (get-buffer "*compilation*")
      (if (get-buffer-window "*compilation*")
          (delete-window (get-buffer-window "*compilation*"))
        (popwin:popup-buffer "*compilation*" :noselect t :stick t :tail t))))
  :bind
  ("C-x m" . popwin:messages)
  ("C-x e" . popwin:flycheck-errors)
  ("C-x c" . popwin:compilation))

(use-package paradox :ensure t :defer 30
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package package-utils :ensure t
  :commands (package-utils-upgrade-by-name))

;; (use-package esup
;;   :ensure t
;;   :commands (esup)
;;   :defer 30)

(use-package restart-emacs
  :ensure t
  :defer 20
  :commands restart-emacs)

(use-package help-fns+ :ensure t :defer 5)

;; dicts
(use-package bing-dict :ensure t
  :commands (bing-dict-brief)
  :defer 20)

(use-package youdao-dictionary :ensure t :defer 20
  :commands (youdao-dictionary-search-at-point))

(use-package el2markdown :ensure t :defer t)  ;; TODO configuration

(use-package chinese-yasdcv :ensure t :defer t
  :commands (yasdcv-translate-at-point)
  :ensure chinese-pyim
  :config
  (setq yasdcv-sdcv-command "sdcv --non-interactive --utf8-output --utf8-input --use-dict \"%dict\" \"%word\"")
  (setq yasdcv-sdcv-dicts
        '(("21century" "21世纪英汉汉英双向词典" "21cen" t))))

(use-package github-browse-file :ensure t :defer t)
(use-package google-this :ensure t :defer t
  :commands (google-this))

(provide 'config-utilities)
