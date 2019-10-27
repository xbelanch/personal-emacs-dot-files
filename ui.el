;; ui.el
(if window-system
    (progn
      ;; UI parts
      (toggle-scroll-bar 0)
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      (add-to-list 'default-frame-alist '(fullscreen . maximized))

      (setq use-default-font-for-symbols nil)
      (setq inhibit-compacting-font-caches t)
      (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 120)))


(use-package all-the-icons)

(use-package posframe)

(use-package popwin)
;; TODO: https://github.com/blue0513/point-history
;; (use-package point-history)

(use-package dashboard
    :diminish
  (dashboard-mode page-break-lines-mode)
  :preface
  (defun my/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
  time and garbage collections."""
    (setq dashboard-banner-logo-title
          (format "Emacs ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done))
    (setq dashboard-footer "Free Belanche Foundation")
    (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face)))

  :config
  (setq dashboard-startup-banner (concat user-emacs-directory "mriocbot.png"))
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  :hook ((after-init     . dashboard-refresh-buffer)
         (dashboard-mode . my/dashboard-banner)))


;; Change cursor style
(add-to-list 'default-frame-alist '(cursor-type . bar))
;; vertical border
(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?�)) ; or ? �
  (setq standard-display-table display-table))
(set-face-background 'vertical-border "#0e0f1b")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(use-package persp-mode
  :disabled
  :diminish
  :commands (get-current-persp persp-contain-buffer-p)
  :hook (after-init . persp-mode))

(use-package imenu-list
  :bind
  ("<f10>" . imenu-list-smart-toggle)
  :custom-face
  (imenu-list-entry-face-1 ((t (:foreground "white"))))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  ;; (vertical-bar   (doom-darken base5 0.4))
  ;; (doom-darken bg 0.4)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; Modeline
  (use-package doom-modeline
    :custom
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    :hook
    (after-init . doom-modeline-mode)
    :config
    (set-cursor-color "cyan")
    (line-number-mode 0)
    (column-number-mode 0)
    (doom-modeline-def-modeline 'main
      '(bar window-number buffer-info buffer-position selection-info)
      '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))))


(use-package nyan-mode
   :custom
   (nyan-cat-face-number 4)
   (nyan-animate-nyancat t)
   :hook
   (doom-modeline-mode . nyan-mode))

(use-package hide-mode-line
  :hook
  ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode))

(use-package display-line-numbers
  :ensure nil
  :hook
  ((prog-mode yaml-mode systemd-mode) . display-line-numbers-mode))

(use-package dimmer
  :disabled
  :custom
  (dimmer-fraction 0.5)
  (dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*Messages.*"
         ".*Async.*"
         ".*Warnings.*"
         ".*LV.*"
         ".*Ilist.*"))
  :config
  (dimmer-mode t))

(use-package fill-column-indicator
  :hook
  ((markdown-mode
    git-commit-mode) . fci-mode))

(use-package presentation)

(setq eshell-prompt-function
      (lambda ()
        (format "%s %s\n%s%s%s "
                (all-the-icons-octicon "repo")
                (propertize (cdr (shrink-path-prompt default-directory)) 'face `(:foreground "white"))
                (propertize "?" 'face `(:foreground "#ff79c6"))
                (propertize "?" 'face `(:foreground "#f1fa8c"))
                (propertize "?" 'face `(:foreground "#50fa7b")))))

(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-cmpl-ignore-case t)
(setq eshell-ask-to-save-history (quote always))
(setq eshell-prompt-regexp "??? ")
(add-hook 'eshell-mode-hook
          '(lambda ()
             (progn
               (define-key eshell-mode-map "\C-a" 'eshell-bol)
               (define-key eshell-mode-map "\C-r" 'counsel-esh-history)
               (define-key eshell-mode-map [up] 'previous-line)
               (define-key eshell-mode-map [down] 'next-line)
               )))