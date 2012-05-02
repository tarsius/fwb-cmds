;;; fwb-cmds.el --- misc frame, window and buffer commands

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Version: 0.2.0
;; Homepage: http://github.com/tarsius/fwb-cmds
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Commands defined here operate on frames, windows and buffers and
;; make it easier and faster to access certain functionality that
;; is already available using the builtin commands.

;;  ***** NOTE: The following EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `delete-window' - If only one window in frame, `delete-frame'.

;; Inspired by Drew Adams' `frame-cmds.el' and `misc-cmds.el'.

;;; Code:

(or (fboundp 'old-delete-window)
    (fset 'old-delete-window (symbol-function 'delete-window)))

;; REPLACES ORIGINAL (built-in):
;; If WINDOW is the only one in its frame, `delete-frame'.
;;;###autoload
(defun delete-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (if window
	(select-window window)
      (setq window (selected-window)))
    (if (one-window-p t)
	(delete-frame)
      (old-delete-window (selected-window)))))

;;;###autoload
(defun kill-this-buffer-and-its-window ()
  "Kill the current buffer and delete its window.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'."
  (interactive)
  (if (menu-bar-non-minibuffer-window-p)
      (let ((buffer (current-buffer)))
	(delete-window (selected-window))
	(kill-buffer buffer))
    (abort-recursive-edit)))

;;;###autoload
(defun kill-other-buffers-and-their-window ()
  "Kill non-current buffers in the selected frame and delete their window.
Only buffers are considered that have a window in the current frame."
  (interactive)
  (dolist (window (window-list nil :exclude-minibuffer))
    (unless (equal window (selected-window))
      (kill-buffer (window-buffer window))
      (old-delete-window window))))

;;;###autoload
(defun replace-current-window-with-frame ()
  "Delete window but show buffer in a newly created frame."
  (interactive)
  (let ((window (selected-window)))
    (switch-to-buffer-other-frame (current-buffer))
    (old-delete-window window)))

;;;###autoload
(defun switch-to-current-buffer-other-frame ()
  "Create new frame with the current buffer."
  (interactive)
  (switch-to-buffer-other-frame (current-buffer)))

(provide 'fwb-cmds)
;;; fwb-cmds.el ends here
