;;; nano-box.el --- One pixel border around buffers     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicolas P. Rougier (inria)

;; Author: Nicolas P. Rougier (inria) <nicolas.rougier@inria.fr>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows to surround a buffer with a one pixel line,
;; taking advantage of fringes, header line and mode line. This
;; requires to have nano-modelne installed as header line.

;;; Example usage

;; ;; This adds a box around any buffer in prog mode
;;
;; (defun my/display-buffer-box (window)
;;   (with-current-buffer (window-buffer window)
;;     (nano-box-on)))
;;
;; (setq display-buffer-alist '(((derived-mode . (prog-mode))
;;                               display-buffer-reuse-window
;;                               display-buffer-same-window
;;                               (body-function . my/display-buffer-box))))

;;; Code:
(require 'nano-modeline)

(defvar-local nano-box-cookies nil
  "Cookies to store local face modifications")

(defvar-local nano-box-state nil
  "State of the box around buffer")

;;;###autoload
(defun nano-box-on ()
  "Install a box around current buffer using fringes, header line and mode
line. This requires to have nano-modelne installed."

  (setq-local nano-box-mode-line-format mode-line-format)
  (setq nano-box-state t
        mode-line-format '(:eval (nano-modeline-element-window-spacing))
        overline-margin 0
        fringes-outside-margins t
        left-margin-width 1
        right-margin-width 1
        left-fringe-width 1
        right-fringe-width 1)
  (setq-local nano-modeline-border-color (face-foreground 'default))
  (mapc #'face-remap-remove-relative nano-box-cookies)
  (let ((color nano-modeline-border-color))
    (setq nano-box-cookies
          (list (face-remap-add-relative 'fringe
                                         `(:background ,color))
                (face-remap-add-relative 'header-line
                                         `(:box (:color ,(face-background 'default) :line-width (0 . 1))
                                                :underline nil :overline ,color))
                (face-remap-add-relative 'mode-line-active
                                         `(:overline ,color))
                (face-remap-add-relative 'mode-line-inactive
                                         `(:overline ,color)))))
  (set-window-margins nil 1 1)
  (when (eq (window-buffer) (current-buffer))
    (set-window-buffer nil (current-buffer))))

;;;###autoload
(defun nano-box-off ()
  "Remove borders from the current buffer."

  (setq-local nano-box-state nil
              left-fringe-width 1
              right-fringe-width 1
              left-margin-width 0
              right-margin-width 0
              mode-line-format nano-box-mode-line-format
              nano-modeline-border-color (face-background 'default))
  (mapc #'face-remap-remove-relative nano-box-cookies)
  (set-window-margins nil 0 0)
  (set-window-buffer nil (current-buffer)))

;;;###autoload
(defun nano-box ()
  "Toggle box depending on local state"
  (interactive)

  (if (not nano-box-state)
      (nano-box-on)
    (nano-box-off))
  (redisplay t))

(provide 'nano-box)
;;; nano-box.el ends here
