;;; core-packages.el ---
(require 'cl)
;; (require 'package)
(setq package-enable-at-startup nil)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-user-dir (local-file-name "elpa"))

;; (package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Requires
(eval-when-compile
  (require 'use-package))
(require 'diminish);; if you use :diminish
(require 'bind-key);; if you use any :bind variant

;; set exec-path
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(provide 'config-package.el)

;;; config-package.el ends here