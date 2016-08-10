(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :init
  (progn
    ;; magit extensions
    (use-package magit-blame
      :bind ("C-c C-g b" . magit-blame-mode))

    (use-package evil-magit
      :ensure t
      :config
      (add-hook 'magit-mode-hook 'evil-local-mode))

    ;; Close git commit window after commit
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))

    ;; Close git commit window after an abort
    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))

    (defun magit-commit-mode-init ()
      (when (looking-at "\n")
        (open-line 1)))
    (add-hook 'git-commit-mode-hook 'magit-commit-mode-init)))
