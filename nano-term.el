;;; nano-term.el --- Quake like console              -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'eat)
(require 'nano-box)
(require 'nano-popup)

(add-hook 'eat-exit-hook (lambda (process)
                           (kill-buffer eat-buffer-name)))
(add-hook 'eat-mode-hook (lambda ()
                           (nano-modeline nano-modeline-format-terminal)
                           (face-remap-add-relative 'header-line '(:background "#f9f9e0"))
                           (face-remap-add-relative 'region '(:background "#f9f9e0"))
                           (face-remap-add-relative 'default '(:background "#fffff0"))))

;;;###autoload
(defun nano-term ()
  "Show/hide eat terminal at the bottom of the frame."

  (interactive)
  (if (get-buffer eat-buffer-name)
      (nano-popup eat-buffer-name)
    (let ((display-buffer-alist `(("\\*eat\\*"
                                   (display-buffer-at-bottom)
                                   (window-height . 12)
                                   (dedicated . t)))))
      (eat-other-window)
      (with-current-buffer eat-buffer-name
        (nano-box-on)))))

(provide 'nano-term)
;;; nano-term.el ends here
