(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :defer
  :config
  (evil-set-initial-state 'neotree-mode 'normal)
  (evil-define-key 'normal neotree-mode-map
    (kbd "RET") 'neotree-enter
    (kbd "c")   'neotree-create-node
    (kbd "r")   'neotree-rename-node
    (kbd "d")   'neotree-delete-node
    (kbd "j")   'neotree-next-line
    (kbd "k")   'neotree-previous-line
    (kbd "g")   'neotree-refresh
    (kbd "C")   'neotree-change-root
    (kbd "I")   'neotree-hidden-file-toggle
    (kbd "H")   'neotree-hidden-file-toggle
    (kbd "q")   'neotree-hide
    (kbd "l")   'neotree-enter
    (kbd "|")   (neotree-make-executor
                 :file-fn 'neo-open-file-vertical-split)
    (kbd "-")   (neotree-make-executor
                 :file-fn 'neo-open-file-horizontal-split)
    ))
