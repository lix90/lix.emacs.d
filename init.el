;;; init --- My Emacs initialization file:

;; This file has been updated according to many awesome dot.emacs.d

;; Author: Xiang, Li <alexiangli@outlook.com>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl)

(defconst package-mirror "emacs-china")
;; (defconst package-mirror "original")
(defconst user-project-directory "~/projects/")
(defconst is-mac (string-equal system-type "darwin"))
(defconst user-cache-directory
  (file-name-as-directory (concat user-emacs-directory ".cache"))
  "My emacs storage area for persistent files.")
(make-directory user-cache-directory t)

(setq package-enable-at-startup nil
      load-prefer-newer t)

(when load-file-name
  (defconst base-path (file-name-directory load-file-name)))

(setq custom-file (concat base-path "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(cond
 ((string-equal package-mirror "emacs-china")
  (setq package-archives
		'(("org" . "https://elpa.emacs-china.org/org/")
		  ("gnu" . "https://elpa.emacs-china.org/gnu/")
		  ("melpa" . "https://elpa.emacs-china.org/melpa/"))))
 ((string-equal package-mirror "original")
  (setq package-archives
		'(("org"       . "https://orgmode.org/elpa/")
		  ("gnu"       . "https://elpa.gnu.org/packages/")
		  ("melpa"     . "https://melpa.org/packages/")))))

(package-initialize nil)
(when (not package-archive-contents) (package-refresh-contents))

(unless (package-installed-p 'org)
 (package-refresh-contents)
 (package-install 'org))

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(setq use-package-verbose t)

(eval-when-compile (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package benchmark-init :ensure t
  :init (require 'benchmark-init))
(add-hook
 'benchmark-init/tree-mode-hook
 '(lambda ()
    (local-set-key "i" '(lambda () (interactive) (find-file user-init-file)))
    (local-set-key "s" '(lambda () (interactive) (switch-to-buffer "*scratch*")))
    (local-set-key "t" 'counsel-load-theme)
    (local-set-key "f" 'counsel-set-font)
    (local-set-key "a" 'org-agenda)
    (local-set-key "p" 'projectile-switch-project)))

(use-package el-get :ensure t :defer t)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(use-package paradox :ensure t :defer 30
  :commands (paradox-list-packages
             paradox-upgrade-packages)
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package package-utils :ensure t :defer 30
  :commands (package-utils-upgrade-by-name))


(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name user-emacs-directory))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))

(dolist (tmp '("basic"
               "appearance"
               "navigation"
               "programming"
               "version-control"
               "shell"
               "data-science"
               "web-development"
               ;;"evil"
               "defuns"
               "keybindings"
               "writing"
               "languages"
               "utilities"))
  (load-file (local-file-name
              (concat "config/config-" tmp ".el"))))

(provide 'init)
;;; init.el ends here
