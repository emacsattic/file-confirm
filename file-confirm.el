;;; file-confirm.el --- Confirm creation of duplicate frames

;; Copyright (c) 2000-2002 Pekka Marjola

;; Author: Pekka Marjola <pema@iki.fi>
;; Keywords: file, frame
;; Version: 0.9

;; This file is not part of XEmacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to pema@iki.fi) or from
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.
;;
;; Send bug reports to pema@iki.fi

;; Created: 2.4.2000
;; $Modified: Mon Oct 28 13:03:51 2002 by pema $

;;; Commentary:

;; Some utility functions to avoid creation of extra frames or buffers
;; unnecessarily.
;;
;; Tested so far only in XEmacs-21.1.4.
;; Requires custom.
;; Most likely does not work in GNU Emacs.

;;; Code:

(defgroup file-confirm nil
  "Confirmation for finding non-existing files and creation of duplicate frames."
  :group 'files
  :prefix "file-confirm-")

;;;###autoload
(defcustom file-confirm-warp-mouse t
  "*If non-nil, warp mouse when using an old frame.
This is contrary to ICCCM, but may be useful."
  :group 'file-confirm
  :type 'boolean)

(defconst file-confirm-version "0.9"
  "Version of file-confirm package.")

(defcustom file-confirm-mode nil
  "*If non-nil, activates file-confirm-mode."
  :type 'boolean
  :require 'file-confirm
  :group 'file-confirm)

;;;###autoload
(defcustom file-confirm-mode-line-string " FC"
  "*Modeline indicator for file-confirm."
  :group 'file-confirm
  :type 'string)

;;;###autoload
(defvar file-confirm-mode-map nil
  "Keymap for file-confirm minor mode.")

(defcustom file-confirm-mode-hook nil
  "*List of functions to call when entering file-confirm minor mode."
  :group 'file-confirm
  :type 'hook)

(defcustom file-confirm-load-hook nil
  "*List of functions to call after loading file-confirm."
  :group 'file-confirm
  :type 'hook)

;;;###autoload
(if file-confirm-mode-map
    nil
  (setq file-confirm-mode-map (make-sparse-keymap "file-confirm-map"))
  (define-key file-confirm-mode-map [(control x) ?5 f] 'find-file-frame-or-file)
  (define-key file-confirm-mode-map [(control x) (control f)] 'find-file-confirm-non-existing)
  (define-key file-confirm-mode-map [(control x) ?5 b] 'switch-to-frame-or-buffer-other-frame))


;;;###autoload
(defun find-file-frame-or-file (file &optional codesys)
  "If a frame is already showing the file, show the frame,
if the file is in a buffer, open a new frame for buffer. Otherwise
open the file in a new frame."
  (interactive "FFind file in other frame: \nZCoding system: ")
  (find-file-frame-or-file-internal file codesys))

(defun find-file-frame-or-file-internal (file &optional codesys)
  (let ((buf (find-buffer-visiting file)))
    (if buf
	;; Must use get-buffer-window-list or sothing that allows specifying
	;; parameters for walk-windows to get all frames. frames-of-buffer
	;; does not work. Insane!
	(let* ((window (get-buffer-window-list buf nil 0))
	       (frm (and window
			(window-frame (car window)))))
	  (if (and frm
		   (y-or-n-p (concat "Show " file " in existing frame instead? ")))
	      (progn
		(make-frame-visible frm)
		;;(focus-frame frm)
		;; Against ICCCM, but...
		(when file-confirm-warp-mouse
		  (while (not (frame-visible-p frm)) (sleep-for .5))
		  (set-mouse-position (car window) 2 2)))
	    (find-file-other-frame file codesys)))
      (if (or (file-exists-p file)
	      (y-or-n-p (concat "File " file " does not exist, visit anyway? ")))
          ;; Really, I should set the focus here, too(?)
	  (find-file-other-frame file codesys)
        ;; fake "dir" argument to get the default prompt
        (find-file-frame-or-file-internal (read-file-name "Find file in other frame: " file file) codesys))
      )))
	
;;;###autoload
(defun find-file-confirm-non-existing (file &optional codesys)
  "Ask for confirmation, before calling find-file for non-existing files."
  (interactive "FFind file: \nZCoding system: ")
  (find-file-confirm-non-existing-internal file codesys))

(defun find-file-confirm-non-existing-internal (file &optional codesys)
  (if (or (file-exists-p file)
	  (y-or-n-p (concat "File " file " does not exist, visit anyway? ")))
      (find-file file)
    ;; fake "dir" argument to get the default prompt
    (find-file-confirm-non-existing-internal (read-file-name "Find file in other frame: " file file) codesys)))
  
;;;###autoload
(defun switch-to-frame-or-buffer-other-frame (buffer)
  "Switch to buffer BUFFER in a newly-created frame."
  (interactive "BSwitch to buffer in other frame: ")
  (let* ((name (get-frame-name-for-buffer buffer))
	 (window (get-buffer-window-list buffer nil 0))
	 (frm (and window
		   (window-frame (car window)))))
    (if (and frm
	     (y-or-n-p (concat "Show " buffer " in existing frame instead? ")))
	t
      (setq frm (make-frame (if name
				(list (cons 'name (symbol-name name))))))
      (pop-to-buffer buffer t frm)
      (setq window (get-buffer-window-list buffer nil 0)))
    (make-frame-visible frm)
    ;;(focus-frame frm)
    ;; Against ICCCM, but...
    (when file-confirm-warp-mouse
      (while (not (frame-visible-p frm)) (sleep-for .5))
      (set-mouse-position (car window) 2 2))
    buffer))

;;;###autoload
(defun file-confirm-mode (&optional arg)
  "Toggle file-confirm minor mode.
With arg, turn file-confirm mode on iff arg is positive. When
file-confirm mode is enabled, the editor asks for confirmation before
opening buffers for non-existing files and tries to use existing
frame, if a frame is already showing requested file or buffer.

Special commands:
\\<file-confirm-mode-map>
key		binding
---		-------

\\[find-file-frame-or-file]		find-file-frame-or-file
\\[find-file-confirm-non-existing]		find-file-confirm-non-existing
\\[switch-to-frame-or-buffer-other-frame]		switch-to-frame-or-buffer-other-frame
"
  (interactive "*P")
  (setq file-confirm-mode
	(if (null arg)
	    (not file-confirm-mode)
    (> (prefix-numeric-value arg) 0)))
  (when file-confirm-mode
    (run-hooks 'file-confirm-mode-hook)))
	
;;;###autoload
(defun turn-on-file-confirm ()
  "Unconditionally turn on File-confirm mode."
  (interactive)
  (file-confirm-mode 1))

;;;###autoload
(when (fboundp 'add-minor-mode)
    ;; XEmacs
    (add-minor-mode 'file-confirm-mode
		    file-confirm-mode-line-string
		    file-confirm-mode-map))

;; Emacs -- don't autoload
(unless (assq 'file-confirm-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(file-confirm-mode file-confirm-mode-line-string)
	      minor-mode-alist)))

(run-hooks 'file-confirm-load-hook)

(provide 'file-confirm)
