;; Completion

(use-package company
  :ensure t
  :defer t
  :config
  (progn
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay .3)
    (use-package company-jedi
      :ensure t
      :config
      (add-to-list 'company-backends 'company-jedi))
    (use-package company-ansible
      :ensure t
      :config
      (add-to-list 'company-backends 'company-ansible)))


  :init (add-hook 'after-init-hook 'global-company-mode))

;; Syntax Checking

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-indication-mode 'left-fringe)
  (custom-set-variables
   '(flycheck-markdown-mdl-rules '("~MD013" "~MD029")))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :init
  (global-flycheck-mode))

;; Other
(setq require-final-newline t)
