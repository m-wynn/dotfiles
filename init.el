;; Much of this configuration shamelessly borrowed from
;; justanull - github.com/JustANull


(eval-when-compile
  (require 'cl))
(require 'package)
(setf package-enable-at-startup nil)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(push '("org" . "http://orgmode.org/elpa/") package-archives)
(package-initialize)
;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setf user-full-name "Matthew Wynn"
      user-mail-address "matthew@matthewwynn.com")
(setf backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(setf custom-file "~/.emacs.d/custom.el")
(setf inhibit-splash-screen t
      inhibit-startup-message t
      initial-major-mode #'fundamental-mode
      initial-scratch-message nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(setf echo-keystrokes 0.1
      visible-bell 1)
(prefer-coding-system 'utf-8-unix)
(setf jit-lock-stealth-time 1)
(setq-default indent-tabs-mode nil)
(use-package flycheck
  :ensure t :pin melpa
  :config (progn
	    (use-package flycheck-rust
	      :ensure t :pin melpa
	      :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
	    (global-flycheck-mode 1)))
(when (or (> emacs-major-version 24)
	  (and (= emacs-major-version 24)
	       (>= emacs-minor-version 4)))
  (use-package magit
    :ensure t :pin melpa
    :bind ("C-c g" . magit-status)
    :config (setf magit-completing-read-function 'magit-ido-completing-read)))
(use-package syntax-subword
  :ensure t :pin melpa
  :config (global-syntax-subword-mode 1))
(use-package writegood-mode
  :ensure t :pin melpa
  :commands (writegood-mode))
;;;; Major modes
(use-package lua-mode
  :ensure t :pin melpa
  :mode "\\.lua\\'"
  :interpreter "lua")
(use-package markdown-mode
  :ensure t :pin melpa
  :mode "\\.md\\'")
(use-package org
  :ensure t :pin org
  :mode ("\\.org\\'" . org-mode)
  :config (setf org-completion-use-ido t
		org-src-fontify-natively t))
(use-package racket-mode
  :ensure t :pin melpa
  :mode "\\.rkt\\'"
  :interpreter "racket")
(use-package rust-mode
  :ensure t :pin melpa
  :mode "\\.rs\\'")
(use-package haskell-mode
  :ensure t :pin melpa
  :mode "\\.hs\\'")
(use-package elpy
  :ensure t :pin melpa
  :config (elpy-enable)
  )
(use-package rainbow-mode
  :ensure t :pin gnu
  :diminish rainbow-mode
  :config (add-hook 'prog-mode-hook #'rainbow-mode)
  )
(use-package rainbow-delimiters
  :ensure t :pin melpa
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )
(use-package powerline
  :ensure t :pin melpa
  :config (powerline-default-theme)
  )
  (use-package moe-theme
    :ensure t :pin melpa
    :config (moe-dark)
    )
(use-package highlight-indent-guides
  :ensure t :pin melpa
  :commands (highlight-indent-guides-mode)
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config (progn
	    (require 'color)
	    (lexical-let* ((bg (face-attribute 'default :background))
			   (should-lighten (>= 0.5 (caddar (list (apply #'color-rgb-to-hsl
									(color-name-to-rgb bg))))))
			   (brighten-amount (if should-lighten 5 -5)))
	      (set-face-attribute 'highlight-indent-guides-even-face nil
				  :background (color-lighten-name bg brighten-amount))
	      (set-face-attribute 'highlight-indent-guides-odd-face nil
				  :background (color-lighten-name bg (* 2 brighten-amount))))))

;;;; Searching
;; https://gist.github.com/johnmastro/508fb22a2b4e1ce754e0
;; A replacement for isearch-delete-char that is more intuitive.
(defun isearch-delete-something ()
  "Delete non-matching text or the last character.
If isearch has a failing match, deletes the failing portion.
If isearch has no failing match, deletes the last character.
If no previous match was done, just beeps."
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setf isearch-string (substring isearch-string
				    0
				    (or (isearch-fail-pos)
					(1- (length isearch-string))))
	  isearch-message (mapconcat #'isearch-text-char-description
				     isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))
(bind-key "<backspace>" #'isearch-delete-something isearch-mode-map)
(use-package anzu
  :ensure t :pin melpa
  :demand
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
	 ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode 1))
(use-package ido
  :config (progn
	    (setf ido-enable-flex-matching t)
	    (ido-everywhere 1)
	    (use-package flx-ido
	      :ensure t :pin melpa
	      :config (flx-ido-mode 1))
	    (use-package ido-ubiquitous
	      :ensure t :pin melpa
	      :config (ido-ubiquitous-mode 1))
	    (use-package ido-vertical-mode
	      :ensure t :pin melpa
	      :config (ido-vertical-mode 1))
	    (use-package smex
	      :ensure t :pin melpa
	      :bind (("M-x" . smex)
		     ("M-X" . smex-major-mode-commands)))
	    (ido-mode 1)))

;;;; Window management
(use-package ace-window
  :ensure t :pin melpa
  :bind (("C-x o" . ace-window))
  :config (setf aw-background nil
                aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
                aw-scope 'frame))
(use-package dedicated
  :ensure t :pin melpa
  :commands (dedicated-mode))

;;;; Whitespace
(use-package ws-butler
  :ensure t :pin melpa
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))
;;;; Hydras and other keybinds
(use-package hydra
  :ensure t :pin melpa
  :config (progn
	    (bind-key "C-c c" (defhydra hydra-clipboard (:color blue :columns 2)
				"Clipboard"
				("k" clipboard-kill-region "kill")
				("y" clipboard-yank "yank")
				("q" nil nil)))
	    (bind-key "C-c m" (defhydra hydra-modes (:color red :columns 2)
				"Modes"
				("d" dedicated-mode "dedicated-mode")
				("f" flycheck-mode "flycheck-mode")
				("g" writegood-mode "writegood-mode")
				("s" flyspell-mode "flyspell-mode")
				("v" visual-line-mode "visual-line-mode")
				("w" ws-butler-mode "ws-butler-mode")
				("q" nil nil :color blue)))
	    (bind-key "C-c z" (defhydra hydra-zoom (:color red :columns 2)
				"Zoom"
				("f" text-scale-decrease "zoom out")
				("j" text-scale-increase "zoom in")
				("r" (text-scale-set 0) "reset")
				("q" nil nil :color blue)))))

;(use-package evil
;  :ensure t :pin melpa
;  )
;(evil-mode 1)
(use-package aggressive-indent
  :ensure t :pin melpa
  :config (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  )
(use-package auto-complete
  :ensure t :pin melpa
  :config (add-hook 'prog-mode-hook #'auto-complete-mode)
  )
(use-package visual-regexp
  :ensure t :pin melpa
  :config (progn (define-key global-map (kbd "C-c r") 'vr/replace)
                 (define-key global-map (kbd "C-c q") 'vr/query-replace))
  )
(use-package gist
  :ensure t :pin melpa
  )
;;; init.el ends here
