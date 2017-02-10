;; Function creating a eshell buffer with fixed frame height
(defun lix--eshell-buffer (&optional height)
  "Create a eshell buffer with fixed height."
  (interactive)
  (setq height 10)
  (let ((orig-win-height (window-height)))
    (when (>= orig-win-height (* height 2))
      (split-window-below height)
      (other-window)))
  (let ((newbuf (get-buffer-create "*eshell*"))
        (height-win (window-height))
        (height- (- height (window-height))))
    (eshell)
    (window-resize (selected-window) height-)))

