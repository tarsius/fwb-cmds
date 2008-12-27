;;; fwb-cmds.el --- misc frame, window and buffer commands

;; Copyright (C) 2008  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoulli.cc>
;; Created: 20080830
;; Updated: 20080830
;; Version: 0.1
;; Homepage: http://artavatar.net
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

;;  Command defined here operate on frames, windows and buffers and
;;  make it easier and faster to access certain functionality that
;;  is already available using the default commands.

;;  This library can be used by itself but was written as a helper
;;  library for library `bob'.

;;; Code:

(require 'frame-cmds)
(require 'misc-cmds)

(defun kill-this-buffer-and-its-windows (&optional only-current-p)
  "Kill current buffer and delete its windows."
  (interactive)
  (kill-buffer-and-its-windows (current-buffer)))
;; TODO should optionally only delete current window
;;      interactively this should be controlled by a external variable

(defun kill-other-buffers-in-frame-and-their-windows ()
  "Kill all buffers showing in the current frame \
but the current and their window.
Only buffers are considered that have a window in the current frame."
(interactive)
(dolist (win (window-list nil "not"))
  (unless (equal win (selected-window))
    (kill-buffer (window-buffer win))
    (old-delete-window win))))
;; XXX very unfinished
;;     see kill-buffer-and-its-windows

(defun kill-other-buffers-in-frame()
  "Kill all buffers but the current.  Windows are preserved.
Only buffers are considered that have a window in the current frame."
(interactive)
(dolist (win (window-list nil "not"))
  (unless (equal win (selected-window))
    (kill-buffer (window-buffer win)))))
;; XXX very unfinished
;;     see kill-buffer-and-its-windows

(defun delete-other-windows-on (buffer))
;; TODO

(defun delete-other-windows-for (buffer)
  "Delete all windows in all frames showing BUFFER."
  (interactive (read-buffer-for-delete-windows))
  (delete-other-windows-on buffer))

(defun new-frame-current-buffer ()
  "Create new frame with current buffer."
  (interactive)
  (switch-to-buffer-other-frame (current-buffer)))

(defun new-frame-previous-buffer ()
  "Create new frame with a previous buffer of current window.

TODO"
  (interactive)) ;;TODO

(defun new-frame-other-buffer (&optional buffer buffer-list)
  "Create new frame and show BUFFER in it.

BUFFER defaults to the buffer defined in user option `default-other-buffer',
which defaults to `*Scratch*'.

You can also cicle through various lists of possible completions.

Cicle to one of these list (described below) using TODO.

Then select a completion using TODO.

The lists of buffers are:
* buffers without windows,
* recent buffers of the current window,
* recent buffers of all windows of the current frame,
* all buffers, or
* all buffers with same major mode as current buffer.

Which list is initially active is controlled through the user option
`default-buffer-list', which defaults to the list of buffer without
windows.

`buffer-lists-for-command-new-frame-other-buffer' can be customized
to add additional (or remove) lists of possible completions.

Non-interactive use:

BUFFER has to be passed as an argument."
  (interactive)) ;;TODO

(defun new-frame-scratch-buffer ()
  "Create new frame with buffer *Scratch*."
  (interactive)
  (switch-to-buffer-other-frame "*Scratch*"))

;; XXX This gives the same result as split-window-vertically
;;     However split-window-horizontally doesnt make sence with wmii;
;;     so i favor this name.  Other new commands in this category
;;     could be:
;;     * new-window-previous-buffer
;;     If this isnt done the new-window-current-buffer should only
;;     be an alias for split-window-vertically
(defun new-window-current-buffer ()
  "Create new window with current buffer."
  (interactive)
  (switch-to-buffer-other-window "*Scratch*"))

(defun new-window-scratch-buffer ()
  "Create new window with buffer *Scratch*."
  (interactive)
  (switch-to-buffer-other-window "*Scratch*"))

(defun delete-some-frame ()
  "Delete some frame."
  (interactive)
  (delete-frame (get-a-frame (read-frame "Frame to make invisible: "))))

(defun replace-current-window-with-frame ()
  "Delete window but show buffer in a newly created frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (delete-window)
    (switch-to-buffer-other-frame buffer)))
;; TODO if frame has only one window
;;      create new frame with it and show last buffer in old frame

(defun replace-some-window-with-frame ())

(defun replace-other-window-with-frame ())

(provide 'fwb-cmds)
;;; fwb-cmds.el ends here
