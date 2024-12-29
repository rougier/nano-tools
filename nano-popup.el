;;; nano-popup.el --- Configurable popup windows     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicolas P. Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
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

;;;###autoload
(defun nano-popup (buffer)
  "Toggle a popup window at the bottom of frame displaying the given
BUFFER. The size fo the window is saved such that toggling the window
does not change the window size."

  (interactive)
  (let ((window (get-buffer-window buffer)))
    (if window
        (progn
          (with-current-buffer buffer
            (setq-local window-height (window-height window)))
          (delete-window window))
      (progn
        (with-current-buffer buffer
          (pop-to-buffer buffer
                         `((display-buffer-at-bottom)
                           ,(when (boundp 'window-height)
                              (cons 'window-height window-height)))))
        (setq-local window-height (window-height (get-buffer-window buffer)))
      (set-window-dedicated-p nil t)))))


(provide 'nano-popup)
;;; nano-popup.el ends here
