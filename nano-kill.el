;;; nano-kill.el --- NANO kill functions             -*- lexical-binding: t; -*-

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

;; nano-kill functions replace Emacs kill functions and ask for
;; confirmation in a very salient way.


;;; Code
(require 'nano-read)

(defun nano-kill-emacs ()
  "Close frame or kill emacs if current frame is the last one."

  (interactive)

  (if (> (length (frame-list)) 1)
      (progn (delete-frame))
    (if (files--buffers-needing-to-be-saved nil)
        (let ((answer
               (nano-read-yes-or-no
                (propertize "WARNING" 'face 'nano-critical-i)
                (propertize (format "Some buffers are modified. What do you want to do?")
                            'face 'nano-default)
                (list (propertize "Cancel" 'face 'nano-default)
                      (concat (propertize "Save & " 'face 'nano-default)
                              (propertize "Quit" 'face 'nano-critical))
                      (propertize "Quit" 'face 'nano-critical)))))
          (cond ((equal answer "Save & Quit")
                 (save-buffers-kill-terminal t))
                ((equal answer "Quit")
                 (kill-emacs))))
      (kill-emacs))))


(defun nano-kill-buffer ()
  "Kill buffer and ask for confirmation if modified"

  (if (and (buffer-modified-p)
           (not (equal (buffer-name) " *Compiler Input*"))
           (not (derived-mode-p '(special-mode
                                  help-mode
                                  elisp-compile-mode
                                  compilation-mode)))
           (or (derived-mode-p '(prog-mode text-mode))))
    (let ((answer
           (nano-read-yes-or-no
            (propertize "WARNING" 'face 'nano-critical-i)
            (concat "Buffer "
                    (propertize (buffer-name) 'face 'bold)
                    " has been modified, kill it anyway?")
            (list (propertize "NO" 'face 'nano-default)
                  (propertize "YES" 'face 'nano-critical)))))
      (when (equal answer "YES")
          (set-buffer-modified-p nil)
          t))
    t))


(provide 'nano-kill)
;;; nano-kill.el ends here
