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

;; 对旧的CL库提供兼容性支持
(require 'cl)

(defconst is-mac (string= system-type "darwin"))

;; 在启动时不自动加载安装的包
(setq package-enable-at-startup nil)
;; 加载较新的文件
(setq load-prefer-newer t)
;; 设定根路径
(when load-file-name
  (defconst base-path (file-name-directory load-file-name)))

;;; 指定custom.el文件路径
(setq custom-file (concat base-path "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; 指定包下载路径
(require 'package)
(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))

;;;----------------------------------------------------------------------
;;; 镜像管理
(defconst package-mirror "emacs-china")
;; (defconst package-mirror "original")
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

;;;----------------------------------------------------------------------
;;; bind-key指南
;;
;; 添加单个键
;;   (bind-key "C-c x" 'my-ctrl-c-x-command)
;; 以覆盖的方式添加键
;;   (bind-key* "<C-return>" 'other-window)
;; 添加单个键至特定的keymap：
;;   (bind-key "C-c x" 'my-ctrl-c-x-command some-other-mode-map)
;; 取消键的绑定
;;   (unbind-key "C-c x" some-other-mode-map)
;; 添加多个键至特定的map
;;    (bind-keys :map dired-mode-map
;;               ("o" . dired-omit-mode)
;;               ("a" . some-custom-dired-function))
;; 根据前缀键设定keymap
;;    (bind-keys :prefix-map my-customize-prefix-map
;;               :prefix "C-c c"
;;               ("f" . customize-face)
;;               ("v" . customize-variable))
;; 以覆盖的方式添加多个键
;;    (bind-keys*
;;     ("C-o" . other-window)
;;     ("C-M-n" . forward-page)
;;     ("C-M-p" . backward-page))
;; 查看用户自定义的键
;;   M-x describe-personal-keybindings
(require 'bind-key)

(use-package benchmark-init :ensure t :defer t :disabled t
  :init (require 'benchmark-init))

(defun my/load-file-alist (list &optional dir-prefix file-suffix)
  (dolist (tmp list)
    (let* ((file-name (concat (or dir-prefix "config/config-") tmp (or file-suffix ".el")))
           (file-path (expand-file-name file-name user-emacs-directory))
           (parent-dir (file-name-directory file-path)))
      (unless (or (not parent-dir)
                  (file-exists-p parent-dir))
        (make-directory parent-dir))
      (load-file file-path))))

(setq modules-load '("best" "defuns"))
(setq modules-load-graphic
      '(;; tools
        "git" "chinese" "shell" "misc"
        "lisp"
        ;; languages
        ;;"javascript" "web" "php"
        ;; "cc"
        ;; "ruby"
        ;; "java"
        "sql"
        "ess-r" "python" "scala"
        ;;"matlab"
        "org" "markdown"
        ;;"latex"
        ))

(my/load-file-alist modules-load)
(when (display-graphic-p)
  (my/load-file-alist modules-load-graphic))

(provide 'init)
;;; init.el ends here



