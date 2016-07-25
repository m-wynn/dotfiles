;; Language Modes

(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :interpreter "python")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.\\(m\\(ark\\)?down\\|md\\)'" . markdown-mode))
  :config
  (progn
    (use-package markdown-preview-mode
      :ensure t
      :config))
  :init (setq markdown-command "multimarkdown"))
