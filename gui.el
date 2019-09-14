;; GUI options
;;----------------------------------------------------------------------------
;; A powerful and beautiful mode-line for Emacs. 
;; Font: https://github.com/Malabarba/smart-mode-line#features
;;----------------------------------------------------------------------------

(use-package powerline
  :init
  (powerline-default-theme))
(add-hook 'after-init-hook 'powerline-reset)

(use-package smart-mode-line-powerline-theme
   :ensure t
   :after powerline
   :after smart-mode-line
   :config
    (sml/setup)
    (sml/theme 'respectful)
    (sml/apply-theme 'powerline)
)


;;----------------------------------------------------------------------------
;; Main Theme: Dracula
;; Source: https://draculatheme.com/emacs/
;;----------------------------------------------------------------------------
(use-package dracula-theme
  :init (load-theme 'dracula t)
  :ensure t)
;; if you want a bit of visual hardcore, uncomment this:
;; (use-package zenburn-theme
;; :init (load-them 'zenburn t)
;; :ensure t)
;;; Set the font
(set-face-attribute 'default nil :font "Iosevka" :height 120)

(setq line-width-characters 77)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "Lorem ipsum dolor est...") ;; Uh, I know what Scratch is for
(setq visible-bell t)             ;; Get rid of the beeps
(menu-bar-mode -1)
(tool-bar-mode -1)
0(scroll-bar-mode -1)
(fringe-mode 1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; smooth scroll
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; prevent mouse scrolling from sucking ass
(setq mouse-wheel-scroll-amount '(0.07))
(setq mouse-wheel-progressive-speed nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Wrap lines at 80 characters
(setq-default fill-column 80)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package treemacs
  :bind
  (("C-c t" . treemacs)
   ("s-a" . treemacs))
  :config
  (setq treemacs-width 35)
  )


