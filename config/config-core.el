(require 'cl)
(setq package-enable-at-startup nil)

;; debug
(setq debug-on-error t)
(setq debug-on-quit t)
;; Increase the garbage collection threshold to decrease startup time
(setq gc-cons-threshold 100000000)

;; Show elapsed start-up time in mini-buffer
;; (let ((emacs-start-time (current-time)))
;;   (add-hook 'emacs-startup-hook
;;             (lambda ()
;;               (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
;;                 (message "[Emacs initialized in %.3fs]" elapsed)))))

;; List package archives and initialize them
;; (setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
(setq load-prefer-newer t)

(require 'package)
(setq package-archives '(
                         ;;("org"       . "http://orgmode.org/elpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")
                         ;;("gnu"       . "http://elpa.gnu.org/packages/")
                         ("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ;;("melpa"     . "https://melpa.org/packages/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ))
(package-initialize)




;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   ;;(setq package-archives '(("gnu" . "http://elpa.zilongshanren.com/gnu/")
;;   ;;                         ("melpa" . "http://elpa.zilongshanren.com/melpa/"))
;;   (add-to-list
;;    'package-archives
;;    ;;'(("gnu" . "https://elpa.zilongshanren.com/gnu/")
;;    ;;   ("melpa" . "https://elpa.zilongshanren.com/melpa/"))
;;    '("melpa" . "http://melpa.org/packages/")
;;    ;; '("popkit" . "https://elpa.popkit.org/packages/")
;;    ;;'("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/help/elpa/")
;;    t)
;;   (package-initialize))

(setq package-user-dir (local-file-name "elpa"))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :init (exec-path-from-shell-initialize))

;; (use-package auto-package-update
;;   :ensure t
;;   :init
;;   (progn
;;     (setq auto-package-update-interval 14)
;;     (setq auto-package-update-delete-old-versions t)
;;     (add-hook 'auto-package-update-before-hook
;;               (lambda () (message "I will update packages now.")))))

(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package esup :ensure t :defer 5)

;; turn debug off
(setq debug-on-error nil)
(setq debug-on-quit nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(defconst user-cache-directory
    (file-name-as-directory (concat user-emacs-directory ".cache"))
    "My emacs storage area for persistent files.")
  ;; create the `user-cache-directory' if it doesn't exist
(make-directory user-cache-directory t)

;;; backup
(let ((auto-save-dir (concat user-cache-directory "auto-save")))
  ;; Move backup file to `~/.emacs.d/.cache/auto-save
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t)))
  ;; Makesure auto-save directory exist
  (when (not (file-exists-p auto-save-dir))
    (make-directory auto-save-dir t)))
(setq create-lockfiles nil)

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

;; garbage collection
(eval-after-load 'minibuffer
  '(progn
     (lexical-let ((default-threshold gc-cons-threshold))
       (defun my/minibuffer-gc-setup-hook ()
         (setq gc-cons-threshold most-positive-fixnum))
       (add-hook 'minibuffer-setup-hook #'my/minibuffer-gc-setup-hook)
       ;; When exit, set back to default threshold
       (defun my/minibuffer-gc-exit-hook ()
         (setq gc-cons-threshold default-threshold))
       (add-hook 'minibuffer-exit-hook #'my/minibuffer-gc-exit-hook))))
(setq garbage-collection-messages t)

;; Time stamps
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp:
 time-stamp-format "Last modified on %04y-%02m-%02d %02H:%02M:%02S (%U)")
(add-hook 'write-file-hooks 'time-stamp)



(provide 'config-core.el)
;;; config-package.el ends here
