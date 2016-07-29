;; Major-mode
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode))

(use-package latex-preview-pane
  :ensure t
  :init (latex-preview-pane-enable))
