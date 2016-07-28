;; Copyright (C) 2007 Leo Shidai Liu
;; Modified by me
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
(defvar xclip-program (executable-find "xclip")
  "Name of XClip program tool.")

(defvar x-select-enable-clipboard t
    "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection.")

(defvar x-last-selected-text-clipboard nil
  "The value of the CLIPBOARD X selection from xclip.")

(defvar x-last-selected-text-primary nil
  "The value of the PRIMARY X selection from xclip.")

(defun x-set-selection (type data)
    "TYPE is a symbol: primary, secondary and clipboard.

See `x-set-selection'."
    (when (and xclip-program (getenv "DISPLAY"))
      (let* ((process-connection-type nil)
	     (proc (start-process "xclip" nil "xclip"
				  "-selection" (symbol-name type))))
	(process-send-string proc data)
	(process-send-eof proc))))

(defun x-select-text (text &optional push)
  "See `x-select-text'."
  (x-set-selection 'primary text)
  (setq x-last-selected-text-primary text)
  (when x-select-enable-clipboard
    (x-set-selection 'clipboard text)
    (setq x-last-selected-text-clipboard text)))

(defun x-selection-value ()
  "See `x-cut-buffer-or-selection-value'."
  (when (and xclip-program (getenv "DISPLAY"))
    (let (clip-text primary-text)
      (when x-select-enable-clipboard
	(setq clip-text (shell-command-to-string "xclip -o -selection clipboard"))
	(setq clip-text
	      (cond ;; check clipboard selection
	       ((or (not clip-text) (string= clip-text ""))
		(setq x-last-selected-text-primary nil))
	       ((eq      clip-text x-last-selected-text-clipboard) nil)
	       ((string= clip-text x-last-selected-text-clipboard)
		;; Record the newer string,
		;; so subsequent calls can use the `eq' test.
		(setq x-last-selected-text-clipboard clip-text)
		nil)
	       (t (setq x-last-selected-text-clipboard clip-text)))))
      (setq primary-text (shell-command-to-string "xclip -o"))
      (setq primary-text
	    (cond ;; check primary selection
	     ((or (not primary-text) (string= primary-text ""))
	      (setq x-last-selected-text-primary nil))
	     ((eq      primary-text x-last-selected-text-primary) nil)
	     ((string= primary-text x-last-selected-text-primary)
	      ;; Record the newer string,
	      ;; so subsequent calls can use the `eq' test.
	      (setq x-last-selected-text-primary primary-text)
	      nil)
	     (t (setq x-last-selected-text-primary primary-text))))
      (or clip-text primary-text))))

;;;###autoload
(defun turn-on-xclip ()
  (interactive)
  (setq interprogram-cut-function 'x-select-text)
  (setq interprogram-paste-function 'x-selection-value))

;;;###autoload
(defun turn-off-xclip ()
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))


(add-hook 'terminal-init-xterm-hook 'turn-on-xclip)
