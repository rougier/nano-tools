;;; nano-journal.el --- Small journal log            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicolas Rougier

;; Author: Nicolas Rougier <rougier@M-E7-NPR.local>
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
(require 'nano-box)
(require 'nano-popup)

(defvar nano-journal-filename "~/Documents/org/journal.org"
  "Journal filename")

(defun nano-journal--last ()
  "Go to the last entry and update time if empty, else, create a new time
entry."

  (interactive)
  (let ((day   (nth 3 (decode-time (current-time))))
        (month (nth 4 (decode-time (current-time))))
        (year  (nth 5 (decode-time (current-time)))))
    (org-with-wide-buffer
     (org-datetree-find-date-create (list month day year))
     (org-narrow-to-subtree))
    (goto-char (point-max))
    (cond ((search-backward (format-time-string "%H:%M") nil t)
           (end-of-line))
          ((search-backward-regexp "[0-9][0-9]:[0-9][0-9] \n" nil t)
           (with-silent-modifications
             (replace-match (format-time-string "%H:%M \n")))
           (backward-char))
          (t
           (goto-char (point-max))
            (with-silent-modifications
              (insert (format-time-string "\n%H:%M \n")))
           (backward-char)))
    (org-narrow-to-element)))

(defun nano-journal--heading ()
  "Return a string describing the current heading (that is supposed to be a
date)."

  (let* ((heading (or (org-with-wide-buffer (org-get-heading)) ""))
         (date (if (eq (org-current-level) 3)
                   (let ((date (date-to-time heading)))
                     (format-time-string "%A, %d %B %Y" date))
                 heading)))
    (propertize (format "%s" date)
                'face 'nano-modeline-face-default)))

(defun nano-journal--setup ()
  "Setup the journal buffer with dedicated font-lock, modeline and a box
around it."
  (let ((buffer (pop-to-buffer (find-file-noselect nano-journal-filename)
                               '((display-buffer-at-bottom)
                                 (window-height . 12)
                                 (dedicated . t)))))
  (with-current-buffer buffer
    (push 'display font-lock-extra-managed-props)
    (font-lock-add-keywords nil
         `(("\\([0-9][0-9]:[0-9][0-9]\\)" 1 '(nano-strong) prepend)
           ("\\(#[A-Za-z0-9]+\\)"         1 '(nano-popout nano-strong) prepend)))
      (font-lock-flush (point-min) (point-max))
      (nano-box-on)
      (setq-local org-element-use-cache nil)
      (nano-modeline (cons '((nano-modeline-element-buffer-status "LOG")
                             nano-modeline-element-space
                             nano-journal--heading
                             nano-modeline-element-space
                             nano-modeline-element-buffer-vc-mode)
                           '(nano-modeline-element-window-status
                             nano-modeline-element-space)))
      (nano-journal--last))))

;;;###autoload
(defun nano-journal ()
  "Show/hide journal and narrow to last entry."

  (interactive)
  (if (get-file-buffer nano-journal-filename)
      (let ((buffer (get-file-buffer nano-journal-filename)))
        (nano-popup (get-file-buffer nano-journal-filename))
        (when (get-buffer-window buffer)
          (with-current-buffer buffer
            (nano-journal--last))))
    (nano-journal--setup)))

(provide 'nano-journal)
;;; nano-journal.el ends here
