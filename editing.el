;; editing.el

;; Ignore split window horizontally
(setq split-width-threshold nil)
(setq split-width-threshold 160)

;; Default Encoding
(prefer-coding-system 'utf-8)
;;(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8) ; nil
(setq process-coding-system-alist
      (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; Quiet Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(defun display-startup-echo-area-message ()
  (message "Me como una hamburguchats"))

(setq frame-title-format nil)
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)    ; Deleting files go to OS's trash folder
(setq make-backup-files nil)          ; Forbide to make backup files
(setq auto-save-default nil)          ; Disable auto save
(setq set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
(setq track-eol t)			; Keep cursor at end of lines.
(setq line-move-visual nil)		; To be required by track-eol
(setq-default kill-whole-line t)	; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space
(defalias 'yes-or-no-p #'y-or-n-p)

(when (functionp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

;; Delete selection if insert someting
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

;; Recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :bind(("C-c r" . counsel-recentf))
  :custom
  (recentf-max-saved-items 20000000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "recentf"
                     "COMMIT_EDITMSG\\'")))

(use-package undo-tree
  :bind
  ("M-/" . undo-tree-redo)
  :config
  (global-undo-tree-mode))

(use-package projectile
  :diminish
  :bind
  ("M-o p" . counsel-projectile-switch-project)
  :config
  (projectile-mode +1))

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag))

;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))


(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;; Move paragraphs or text like Sublime Text
(unless (package-installed-p 'move-text)
  (package-refresh-contents)
  (package-install 'move-text))

(use-package move-text
  :bind
  ("M-<down>" . move-text-down)
  ("M-<up>" . move-text-up))

(use-package duplicate-thing
  :bind ("C-c C-d" . duplicate-thing))

;;; Warnings, Alerts and other special keywords in comments
;; from Casey Muratori
(setq fixme-modes '(c-mode js2-mode yaml-mode sgml-mode fountain-mode markdown-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-done-face)
(make-face 'font-lock-alert-face)
(make-face 'font-lock-hack-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\):" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\):" 1 'font-lock-note-face t)
           ("\\<\\(HACK\\):" 1 'font-lock-alert-face t)
           ("\\<\\(DONE\\):" 1 'font-lock-done-face t)
           ("\\<\\(FIXME\\):" 1 'font-lock-hack-face t)
	   ("\\<\\(ALERT\\):" 1 'font-lock-alert-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "magenta" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "cyan" nil nil t nil t nil nil)
(modify-face 'font-lock-done-face "green" nil nil t nil t nil nil)
(modify-face 'font-lock-alert-face "OrangeRed" nil nil t nil t nil nil)
(modify-face 'font-lock-hack-face "gold" nil nil t nil t nil nil)
