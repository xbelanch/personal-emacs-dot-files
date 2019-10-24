;; misc useful keybindings
(bind-key "C-c C-b" 'eval-buffer)
(bind-key "C-z" 'undo)
(bind-key "C-c f" 'find-file)
(bind-key "C-x e" 'other-frame)
(bind-key "C-c C-v" 'revert-buffer)
(bind-key "C-c <left>" 'windmove-left)
(bind-key "C-c <right>" 'windmove-right)
(bind-key "C-c <up>" 'windmove-up)
(bind-key "C-c <down>" 'windmove-down)
(bind-key "C-q" 'kill-buffer)
(bind-key "M-q" 'query-replace-regexp)
(bind-key "s-<" 'beginning-of-buffer)
(bind-key "s->" 'end-of-buffer)
(bind-key "C-c C-w" 'fill-paragraph)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package hydra)
