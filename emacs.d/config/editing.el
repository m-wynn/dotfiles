
; Completion

(use-package company
  :ensure t
  :defer t
  :config (progn
	(use-package company-jedi
  :config
  (add-to-list 'company-backends 'company-jedi)))
  :init (add-hook 'after-init-hook 'global-company-mode))

;; Syntax Checking

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-indication-mode 'left-fringe)
  (custom-set-variables
   '(flycheck-markdown-mdl-rules '("~MD013" "~MD029")))
  :init
  (global-flycheck-mode))

