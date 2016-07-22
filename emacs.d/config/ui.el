;; Turn off gross defaults
(setf inhibit-splash-screen t                   ; Don't show splash screen
      inhibit-startup-message t                 ; Don't show status message
      initial-major-mode #'fundamental-mode     ; Start in most basic of modes
      initial-scratch-message nil)              ; Don't prepopulate scratch
(global-linum-mode t)                           ; Show line numbers
(menu-bar-mode -1)                              ; Disable menu bar
(if (display-graphic-p)
  (progn
    (scroll-bar-mode -1)                            ; Disable scroll bar
    (tool-bar-mode -1)                              ; Disable toolbar
    )
  )

(show-paren-mode t)                             ; Always highlight matching character pairs

;; Transparent bg
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(defalias 'yes-or-no-p 'y-or-n-p); Let 'y' and 'n' suffice for yes/no


;; Themes and colors
(use-package powerline
             :ensure t
             :config (powerline-center-evil-theme)
             )

(use-package moe-theme
             :ensure t
             :config (moe-dark)
             )

(powerline-moe-theme)
