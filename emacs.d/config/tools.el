;; Terminal in emacs
(use-package multi-term
  :ensure t
  :config 
  (setq multi-term-program "/bin/zsh"))

;; Paste to ix.io
(use-package ix
  :ensure t)

;; TRAMP
(use-package tramp
  :ensure t)
  
