;;; init.el
;; Debugging
(setq message-log-max 10000)

(setq my-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path my-lisp-dir)
(let ((default-directory my-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Package management
;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Requires
(eval-when-compile
  (require 'use-package))
(require 'config-keybind)
(require 'diminish)
(require 'config-looking)
(require 'config-edit)
;; language
(require 'config-lang)
;; tool
(require 'config-tool)

;;; end init.el
