;; History
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      savehist-file (concat user-emacs-directory "history"))
(savehist-mode 1)

;; Persistant undo history across sessions
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))))
    :init (global-undo-tree-mode))


;; Auto-saves and backups
(defconst emacs-backup-dir (format "%s/%s/" user-emacs-directory "backups"))
(setq backup-by-copying t   ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-directory-alist `((".*" . ,emacs-backup-dir))
      auto-save-file-name-transforms `((".*" ,emacs-backup-dir t)))

;; Simultanious editing
(setq create-lockfiles nil)   ; Don't leave .#filename files around
