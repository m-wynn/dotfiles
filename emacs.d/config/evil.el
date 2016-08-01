(defun easy-align ()
  (interactive)
  (let ((BEG (region-beginning))
	(END (region-end)))
        (align-regexp BEG END "\\(\\s-*\\)\\s-" 1 1 t)))

(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-default-cursor t)	; Don't overwrite cursor color
    (evil-mode 1)
    )
  :config
  (progn
    ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    ;; Easy-align
    (define-key evil-visual-state-map "ga" 'easy-align)
    )
  )
