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

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :config
  (progn
    (use-package php-ext
      :ensure t)))

(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :interpreter "python")

(use-package web-mode
  :ensure t
  :mode "\\.html\\.php\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

