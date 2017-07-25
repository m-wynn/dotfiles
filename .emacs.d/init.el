;;; Load initial things

;; Package management

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load (expand-file-name "packages.el" user-emacs-directory))

;; Emacs scripts and utilities used elsewhere
(load (expand-file-name "elisp-utils.el" user-emacs-directory))

;;; Load everything else
(defun load-directory (directory)
  "Recursively get all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
       (fullpath (concat directory "/" path))
       (isdir (car (cdr element)))
       (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
    (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
    (load (file-name-sans-extension fullpath)))))))

(load-directory "~/.emacs.d/config")
