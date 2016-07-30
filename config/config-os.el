;; 1. 判定系统
(defconst be-macos (eq system-type 'darwin))
(defconst be-linux (eq system-type 'gnu/linux))
(cond
 (be-macos
  (progn (message "System: MacOS")))
 (be-linux
  (progn (message "System: Linux"))))

;; 设置环境变量 exec-path (emacs 的 exec-path 相当于 shell 的 $path)
;; MacOS does not run a shell during the login
;; (cond (be-macos
;;       (progn (exec-path-from-shell-initialize))))

;; 2. 交互设置
;; 键盘和字体
(cond
 (be-macos
  (progn
    ;; 重映射Meta键
    (setq mac-option-modifier 'alt)
    (setq mac-command-modifier 'meta)
    ;; sets fn-delete to be right-delete
    (global-set-key [kp-delete] 'delete-char)
    ;; 中文字体
    (set-fontset-font "fontset-default" 'gb18030' ("STHeiti" . "unicode-bmp"))
    )))

;; 窗口最大化
;; (cond
;;  ;; MacOS F11全屏 F10最大化
;;  (be-macos
;;   (progn
;;     (defun toggle-fullscreen ()
;;       (interactive)
;;       (set-frame-parameter
;;        nil 'fullscreen
;;        (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
;;     (require 'maxframe)
;;     (defvar my-fullscreen-p t "Check if fullscreen is on or off")
;;     (defun toggle-maxfram ()
;;       (interactive)
;;       (setq my-fullscreen-p (not my-fullscreen-p))
;;       (if my-fullscreen-p
;;           (restore-frame)
;;         (maximize-frame)))
;;     (global-set-key [M-f11] 'toggle-fullscreen)
;;     (global-set-key [M-f10] 'toggle-maxfram)))
;;  ;; Linux 启动最大化
;;  (be-linux
;;   (progn
;;     (defun start-maximized ()
;;       (interactive)
;;       (x-send-client-message
;;        nil 0 nil "_NET_WM_STATE" 32
;;        '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;       (interactive)
;;       (x-send-client-message
;;        nil 0 nil "_NET_WM_STATE" 32
;;        '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
;;     (start-maximized))))

;; 3. 调用外部程序设置
;; 浏览器设置 Chrome
;; (cond
;;  (be-macos
;;   (progn
;;     (setq browse-url-generic-program "open")))
;;  (be-linux
;;   (progn
;;     (setq browse-url-browser-function 'browse-url-generic
;;           browse-url-generic-program "google-chrome"))))

;; Dired 关联外部程序
;; (require 'run-assoc)
;; (cond
;; shell 命令 open 可以打开任意 MacOS App
;; (be-macos
;;  (progn
;;    (setq associated-program-alist
;;          '(("open" "\\..*$\\|\\.ps$")))
;;    ))
;; (be-linux
;;  (progn
;;    (setq associated-program-alist
;;          '(((lambda (file) (browse-url (concat "file:///" (expand-file-name file)))) "\\.html?$")
;;            ("evince" "\\.pdf$\\|\\.ps$")
;;            ("kchmviewer" "\\.chm$")
;;            ("openoffice.org" "\\.doc$\\|\\.docx$\\|\\.ppt$\\|\\.pptx$")
;;            ("eog" "\\.jpg$\\|\\.png$\\|\\.bmp$\\|\\.gif$")
;;            ("totem" "\\.avi$\\|.\\.mpeg$\\|\\.rm$\\|\\.wmv$\\|\\.mp3$")))
;;    )))

(provide 'config-os)
