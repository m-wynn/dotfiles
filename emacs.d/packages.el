(eval-when-compile
  (require 'package))

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               ;; For important compatibility libraries like cl-lib
               '("gnu" . "http://elpa.gnu.org/packages/") t)
  )

;; Make sure packages archives have been downloaded
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)        ; Allow removal of clutter
(require 'bind-key)        ; Allow binding of keys
