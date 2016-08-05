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

;; Folding
(defun define-fringe-bitmap (bits &optional height width align bitmap)
  nil)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(use-package hideshowvis
  :ensure t
  :config
  (progn
    (hideshowvis-enable)
    (hideshowvis-symbols)))

;; Other
(setq require-final-newline t)

(setq indent-tabs-mode nil)
