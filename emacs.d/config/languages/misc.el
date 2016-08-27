;; Language Modes

(use-package dockerfile-mode
  :ensure t
  :mode "\\.Dockerfile\\'")

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.\\(m\\(ark\\)?down\\|md\\)'" . markdown-mode))
  :config
  (progn
    (use-package flymd
      :ensure t
      :config))
  :init (setq markdown-command "multimarkdown"))

(use-package nginx-mode
  :ensure t
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/"))

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :config
  (progn
    (use-package php-ext
      :ensure t)))

(use-package puppet-mode
  :ensure t
  :mode "\\.pp\\'")

(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :interpreter "python")

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package web-mode
  :ensure t
  :mode "\\.html\\(\\.php\\)?\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package jinja2-mode
  :ensure t
  :mode "templates/")

(use-package ssh-config-mode
  :ensure t
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
  ("/sshd?_config\\'"      . ssh-config-mode)
  ("/known_hosts\\'"       . ssh-known-hosts-mode)
  ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)))
