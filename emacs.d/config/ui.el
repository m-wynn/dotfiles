;; Turn off gross defaults
(setf inhibit-splash-screen t                   ; Don't show splash screen
      inhibit-startup-message t                 ; Don't show status message
      initial-major-mode #'fundamental-mode     ; Start in most basic of modes
      initial-scratch-message nil)              ; Don't prepopulate scratch
(menu-bar-mode -1)                              ; Disable menu bar
(if (display-graphic-p)
  (progn
    (scroll-bar-mode -1)                        ; Disable scroll bar
    (tool-bar-mode -1)                          ; Disable toolbar
    )
  )

(show-paren-mode t)                             ; Always highlight matching character pairs

(defalias 'yes-or-no-p 'y-or-n-p)               ; Let 'y' and 'n' suffice for yes/no
(setq-default word-wrap t)			; Word wrap

(setq show-trailing-whitespace t)

;; Transparent bg
(defun on-frame-open (&optional frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

;; Line Numbers
(use-package nlinum-relative
  :ensure t
  :init
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-format "%d ")
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (add-hook 'text-mode-hook 'nlinum-relative-mode))


;; Themes and colors
(use-package powerline
  :ensure t
  :config (powerline-center-evil-theme))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (color-theme-sanityinc-tomorrow--define-theme night))

(on-frame-open (selected-frame)) ; Remove background color on load-file
(add-hook 'after-make-frame-functions 'on-frame-open) ; Remove background color on emacsclient load
(add-hook 'window-setup-hook '(on-frame-open (selected-frame))) ; Remove background color on emacs load
