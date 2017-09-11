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
(defconst is-mac (string= system-type "darwin"))

(setq package-enable-at-startup nil
      ;; load-prefer-newer t ; already in better-default
      )

(when load-file-name
  (defconst base-path (file-name-directory load-file-name)))

(setq custom-file
      (concat base-path "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'package)
(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))

(cond
 ((string= package-mirror "emacs-china")
  (setq package-archives
		'(("org" . "https://elpa.emacs-china.org/org/")
		  ("gnu" . "https://elpa.emacs-china.org/gnu/")
		  ("melpa" . "https://elpa.emacs-china.org/melpa/"))))
 ((string= package-mirror "original")
  (setq package-archives
		'(("org"       . "https://orgmode.org/elpa/")
		  ("gnu"       . "https://elpa.gnu.org/packages/")
		  ("melpa"     . "https://melpa.org/packages/")))))

(package-initialize nil)
(when (not package-archive-contents)
  (package-refresh-contents))

;; (unless (package-installed-p 'org)
;;   (package-refresh-contents)
;;   (package-install 'org))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(require 'diminish)
(require 'bind-key)

(use-package benchmark-init :ensure t
  :init
  (require 'benchmark-init))

(defun lix/load-file-alist (list &optional dir-prefix file-suffix)
  (dolist (tmp list)
    (let* ((file-name (concat (or dir-prefix "config/config-") tmp (or file-suffix ".el")))
           (file-path (expand-file-name file-name user-emacs-directory))
           (parent-dir (file-name-directory file-path)))
      (unless (or (not parent-dir)
                  (file-exists-p parent-dir))
        (make-directory parent-dir))
      (load-file file-path))))

(setq modules-load
      '("best"
        "defuns"
        ))

(setq modules-load-graphic
      '("git"
        "chinese"
        "shell"
        ;;"javascript"
        ;;"web"
        "ess-r"
        "python"
        ;;"matlab"
        "scala"
        ;;"org"
        "markdown"
        ;;"latex"
        "lisp"
        ;;"english"
        "misc"
        ;;"php"
        ;;"cc"
        ;; "ruby"
        ;;"java"
        "sql"
        ))

(lix/load-file-alist modules-load)
(when (display-graphic-p)
  (lix/load-file-alist modules-load-graphic))

(dolist (imode '(
                 abbrev-mode
                 blink-cursor-mode
                 delete-selection-mode
                 electric-indent-mode
                 which-key-mode
                 smartparens-mode
                 show-smartparens-mode
                 show-paren-mode
                 whitespace-cleanup-mode
                 smooth-scrolling-mode
                 pangu-spacing-mode
                 line-number-mode
                 ace-pinyin-mode
                 ace-pinyin-global-mode
                 aggressive-indent-mode
                 anzu-mode
                 auto-composition-mode
                 auto-compression-mode
                 company-mode
                 company-flx-mode
                 diff-auto-refine-mode
                 file-name-shadow-mode
                 font-lock-mode
                 global-font-lock-mode
                 eldoc-mode
                 global-eldoc-mode
                 hungry-delete-mode
                 global-hungry-delete-mode
                 subword-mode
                 global-subword-mode
                 ivy-mode
                 mouse-wheel-mode
                 override-global-mode
                 projectile-mode
                 rainbow-delimiters-mode
                 recentf-mode
                 save-place-mode
                 savehist-mode
                 shell-dirtrack-mode
                 super-save-mode
                 tooltip-mode
                 transient-mark-mode
                 undo-tree-mode
                 ))
  (diminish imode))

(provide 'init)
;;; init.el ends here
(put 'erase-buffer 'disabled nil)
