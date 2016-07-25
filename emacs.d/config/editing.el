;; Completion

(use-package company
  :ensure t
  :defer t
  :config
  (progn
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
  :ensure t :pin melpa
  :config
  (progn
    (use-package flycheck-rust
      :ensure t :pin melpa
      :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
    (global-flycheck-mode 1)))

