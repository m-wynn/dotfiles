;;; Emacs-lisp utilities and functions used in other configuration

;; Add a function to multiple hooks
;; http://stackoverflow.com/a/7400476
(defun add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
                hooks))
