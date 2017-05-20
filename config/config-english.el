;;;--------------------
;; English Learning
;;;--------------------
(use-package bing-dict :ensure t :defer t
  :init
  (defalias 'Bing-dictionary 'bing-dict-brief)
  :config
  (require 'thingatpt)
  (defun qjp-search-word-at-mouse ()
    "Search the word at mouse using `bing-dict-brief'."
    (interactive)
    (save-excursion
      (mouse-set-point last-input-event)
      (let ((word (word-at-point)))
        (when word
          (bing-dict-brief word)))))
  (global-set-key (kbd "<C-mouse-1>") 'qjp-search-word-at-mouse)
  (global-unset-key (kbd "<C-down-mouse-1>")))

(use-package youdao-dictionary :ensure t :defer t
  :init
  (defalias 'Youdao-dictionary 'youdao-dictionary-search-at-point))

(use-package wotd :ensure t :defer t)

(provide 'config-english)
