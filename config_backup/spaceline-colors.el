;;; spaceline-colors.el --- Color theming for custom ATI spaceline

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

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

(defvar spaceline--theme-colors-alist
  '((;; Dark themes
     atom-one-dark  ((active   . ((highlight . ((background . "#98C379") (foreground . "#1d2021")))
                                  (default   . ((background . "#3E4451") (foreground . "#AAAAAA")))
                                  (other     . ((background . "#3e4451") (foreground . "#AAAAAA")))
                                  (middle    . ((background . "#3e4451")))))
                     (inactive . ((highlight   . ((background . "#1d2021") (foreground . "#666D7A")))
                                  (default   . ((background . "#1d2021") (foreground . "#666D7A")))
                                  (other     . ((background . "#1d2021") (foreground . "#666D7A")))
                                  (middle    . ((background . "#1d2021")))))))
    (doom-one ((active   . ((highlight . ((background . "#1f5582") (foreground . "#eeeeee")))
                            (default   . ((background . "#404850") (foreground . "#eeeeee")))
                            (other     . ((background . "#404850") (foreground . "#eeeeee")))
                            (middle    . ((background . "#3d3d48")))));
               (inactive . ((default   . ((background . "#181e26") (foreground . "#555555")))
                            (other     . ((background . "#181e26") (foreground . "#555555")))
                            (middle    . ((background . "#181e26")))))))
    (doom-molokai ((active   . ((highlight . ((background . "#1f5582") (foreground . "#eeeeee")))
                                (default   . ((background . "#404850") (foreground . "#eeeeee")))
                                (other     . ((background . "#404850") (foreground . "#eeeeee")))
                                (middle    . ((background . "#3d3d48")))));
                   (inactive . ((highlight   . ((background . "#3d3d48") (foreground . "#888888")))
                                (default   . ((background . "#3d3d48") (foreground . "#888888")))
                                (other     . ((background . "#3d3d48") (foreground . "#888888")))
                                (middle    . ((background . "#3d3d48")))))))
    (default-theme ((active   . ((highlight . ((background . "#1f5582") (foreground . "#eeeeee")))
                                 (default   . ((background . "#404850") (foreground . "#eeeeee")))
                                 (other     . ((background . "#404850") (foreground . "#eeeeee")))
                                 (middle    . ((background . "#3d3d48")))));
                    (inactive . ((highlight . ((background . "#3d3d48") (foreground . "#888")))
                                 (default   . ((background . "#3d3d48") (foreground . "#888")))
                                 (other     . ((background . "#3d3d48") (foreground . "#888")))
                                 (middle    . ((background . "#3d3d48") (foreground . "#888")))))))
    ;; Light themes
    (doom-one-light ((active   . ((highlight . ((background . "#1f5582") (foreground . "#eeeeee")))
                                  (default   . ((background . "#404850") (foreground . "#eeeeee")))
                                  (other     . ((background . "#404850") (foreground . "#eeeeee")))
                                  (middle    . ((background . "#3d3d48")))));
                     (inactive . ((default   . ((background . "#3d3d48") (foreground . "#888888")))
                                  (other     . ((background . "#3d3d48") (foreground . "#888888")))
                                  (middle    . ((background . "#3d3d48")))))))
    (twilight-bright ((active   . ((highlight . ((background . "#6b82a7") (foreground . "#edf2e9")))
                                   (default   . ((background . "#e3f4ff") (foreground . "#655370")))
                                   (other     . ((background . "#e3f8ff") (foreground . "#655370")))
                                   (middle    . ((background . "#e3ffff")))));
                      (inactive . ((default   . ((background . "#f1f4f8") (foreground . "#6b82a7")))
                                   (other     . ((background . "#f1f4f8") (foreground . "#6b82a7")))
                                   (middle    . ((background . "#f1f4f8")))))))
    (sanityinc-tomorrow-light ((active   . ((highlight . ((background . "#6b82a7") (foreground . "#edf2e9")))
                                            (default   . ((background . "#e3f4ff") (foreground . "#655370")))
                                            (other     . ((background . "#e3f8ff") (foreground . "#655370")))
                                            (middle    . ((background . "#e3ffff")))));

                               (inactive . ((default   . ((background . "#f1f4f8") (foreground . "#6b82a7")))
                                            (other     . ((background . "#f1f4f8") (foreground . "#6b82a7")))
                                            (middle    . ((background . "#f1f4f8")))))))
    ))

(defun spaceline--set-face (face alist)
  "Set FACE to be the foreground & background defined in ALIST."
  (let-alist alist (set-face-attribute face nil :foreground .foreground :background .background)))

(defun spaceline-update-faces (&rest args)
  "Update the faces for the current theme from `custom-enabled-themes'.
ARGS is needed to allow for this function to be used as advice"
  (let ((theme-alist (cadr (assoc (car custom-enabled-themes) spaceline--theme-colors-alist)))
        (theme-default (cadr (assoc 'default-theme spaceline--theme-colors-alist))))
    (when theme-alist
      (let-alist theme-alist
        (spaceline--set-face 'spaceline-highlight-face  .active.highlight)
        (spaceline--set-face 'powerline-active2         .active.middle)
        (spaceline--set-face 'mode-line                 .active.other)
        (spaceline--set-face 'powerline-active1         .active.default)

        (spaceline--set-face 'powerline-inactive1       .inactive.default)
        (spaceline--set-face 'mode-line-inactive        .inactive.other)
        (spaceline--set-face 'powerline-inactive2       .inactive.middle)))
    (when (not theme-alist)
      (let-alist theme-default
        (spaceline--set-face 'spaceline-highlight-face  .active.highlight)
        (spaceline--set-face 'powerline-active2         .active.middle)
        (spaceline--set-face 'mode-line                 .active.other)
        (spaceline--set-face 'powerline-active1         .active.default)

        (spaceline--set-face 'powerline-inactive1       .inactive.default)
        (spaceline--set-face 'mode-line-inactive        .inactive.other)
        (spaceline--set-face 'powerline-inactive2       .inactive.middle)
        ))
    (set-face-attribute 'mode-line nil
                        :box nil
                        :underline nil)
    (set-face-attribute 'mode-line-inactive nil
                        :box nil
                        :underline nil)))

(provide 'spaceline-colors)
;;; spaceline-colors.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
