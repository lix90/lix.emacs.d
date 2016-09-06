;; -----------------------------------
;; key bindings
;; -----------------------------------
(global-set-key (kbd "C-h") nil)
(bind-key "<f1>" #'help-command)
(bind-key "C-h" #'delete-backward-char)
(bind-key "M-h" #'backward-kill-word)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key (kbd "C-c C-e")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c C-j") 'join-line)
;; (global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-c r") 'replace-regexp)
(global-set-key (kbd "C-c m") 'set-mark-command)
(global-set-key [f7] 'eval-region)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "RET") 'newline-and-indent)
;; -----------------------------------
;; insert data
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))

(global-set-key (kbd "C-c d") 'my-insert-date)
;;-----------------------------------
;; resize window
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;;--------------------------------------------------------------------
;; set other window
;; next window
;; (defun other-window-backward ( )
;;   "select the previous window."
;;   (interactive)
;;   (other-window -1))
;; (global-set-key "\C-x\C-n" 'other-window)
;; (global-set-key "\C-x\C-p" 'other-window-backward)
;;--------------------------------------------------------------------
;; change default keybindings
;; (global-set-key "\M-?" 'help-command)
;; (global-set-key "\C-h" 'delete-backward-char)
;;--------------------------------------------------------------------
;; move line
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key "\M-p" 'move-text-up)
(global-set-key "\M-n" 'move-text-down)
;;--------------------------------------------------------------------
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)
;; -------------------------------------------------------------------
;; windowMove
;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings))

;; (defun ignore-error-wrapper (fn)
;;   "Funtion return new function that ignore errors.
;;    The function wraps a function with `ignore-errors' macro."
;;   (lexical-let ((fn fn))
;;     (lambda ()
;;       (interactive)
;;       (ignore-errors
;;         (funcall fn)))))
;; (global-set-key [s-left] (ignore-error-wrapper 'windmove-left))
;; (global-set-key [s-right] (ignore-error-wrapper 'windmove-right))
;; (global-set-key [s-up] (ignore-error-wrapper 'windmove-up))
;; (global-set-key [s-down] (ignore-error-wrapper 'windmove-down))
;; ------------------------------------------------------------------
;; easy comment
(defun my-comment-or-uncomment-region (beg end &optional arg)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2))))
  (comment-or-uncomment-region beg end arg))

(global-set-key [remap comment-or-uncomment-region] 'my-comment-or-uncomment-region)

(global-set-key [?\C-c ?\C-/] 'comment-or-uncomment-region)
;;--------------------------------------------------------------------
;; kill all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c k") 'close-all-buffers)

;;--------------------------------------------------------------------
(provide 'config-keybind)
