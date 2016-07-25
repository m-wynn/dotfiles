(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file (concat user-emacs-directory "history"))
(savehist-mode 1)

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))))
    :init (global-undo-tree-mode))


  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory "backup"))))
  (setq auto-save-file-name-transforms
	`(("." ,(concat user-emacs-directory "auto-save") t)))
