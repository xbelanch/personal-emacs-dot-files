;;; -*- lexical-binding: t; -*-

;; Defer garbage collection
(setq gc-cons-threshold 100000000)

;; Disable package-enable-at-startup
(setq package-enable-at-startup nil)

;; Unset file-name-handler-alist
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable site-run-file
(setq site-run-file nil)

;; Garbage Collection
;;; A large gc-cons-threshold may cause freezing and stuttering during long-term interactive use.
(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))


;; Add Melpa package list
;;; Fix SSL TLS issue when emacs version is under 26.3
(when (and (>= libgnutls-version 30603)
           (version<= (number-to-string emacs-major-version) "26.2"))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)

(setq package-archives '())
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(setq package-archive-priorities
      '(("gnu"          . 10)
        ("melpa"        . 20)
        ("org"          . 30)
        ("melpa-stable" . 0)))

;;; Now we are ready to initialize package system.
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-compute-statistics t)

;; Use the garbage collector magic hack
(use-package gcmh
  :init
  (gcmh-mode 1))

;; Allow some things that emacs would otherwise confirm.
(put 'eval-expression  'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column  'disabled nil)


(setq-default inhibit-splash-screen t
	          inhibit-startup-message t
	          tab-width 4
	          indent-tabs-mode nil
	          compilation-scroll-output t
              compile-command "make -B"
	          debug-on-error t
	          message-log-max 500
	          warning-suppress-types nil
	          display-time-default-load-average nil
	          cursor-in-non-selected-windows t
	          fill-column 80
	          use-package-always-ensure t
	          vc-follow-symlinks t
	          electric-pair-preserve-balance nil
	          global-auto-revert-mode t
	          auto-save-interval  2048
	          blink-matching-paren-distance 100000
	          view-read-only t
	          sentence-end-double-space nil
              confirm-kill-emacs 'y-or-n-p
              column-number-mode 1                            ; Show the column number
              display-time-mode 1                             ; Enable time in the mode-line
              show-paren-mode 1                              ; Show the parent
              visible-bell (equal system-type 'windows-nt))

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; from https://github.com/rexim/dotfiles/blob/master/.emacs.rc/misc-rc.el
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

(global-set-key (kbd "C-,") 'rc/duplicate-line)

;; Set language environment to Catalan
(set-language-environment "Catalan")

;; MULtilingual Enhancement to Emacs
(use-package mule
  :ensure nil
  :config
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  ;; src: https://stackoverflow.com/questions/22647517/emacs-encoding-of-pasted-text
  (if (eq system-type 'windows-nt)
      (set-clipboard-coding-system 'utf-16le-dos))
  (setq locale-coding-system   'utf-8)
  (setq-default buffer-file-coding-system 'utf-8))

;; stolen from https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Shift + Enter for inser new line and jump to it
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))
(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c i m") 'imenu)


;; Fonts and Icons
;; Grabbed from @tsoding: https://github.com/rexim/dotfiles/blob/e5db0dec316c24197c4c3fd32140f467584fa7b3/.emacs
(defun my/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Fantasque Sans Mono-14")
   ((eq system-type 'gnu/linux) "Ubuntu Mono-16")))

(add-to-list 'default-frame-alist `(font . ,(my/get-default-font)))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-faicon)
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))

;; GUI / Window
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(add-to-list 'custom-theme-load-path
             "~/.emacs.d/gruber-darker-theme/")
(load-theme 'gruber-darker t)

;; Ido
(use-package smex)
(use-package ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Display relative line numbers
(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type (quote relative)))

;; Multiple cursors
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;;; dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")

(defun dired-duplicate-this-file ()
  "Duplicate file on this line."
  (interactive)
  (let* ((this  (dired-get-filename t))
         (ctr   1)
         (new   (format "%s[%d]" this ctr)))
    (while (file-exists-p new)
      (setq ctr  (1+ ctr)
            new  (format "%s[%d]" this ctr)))
     (dired-copy-file this new nil))
  (revert-buffer))


;; helm
(use-package helm)
(use-package helm-cmd-t)
(use-package helm-git-grep)
(use-package helm-ls-git)

(setq helm-ff-transformer-show-only-basename nil)
(global-set-key (kbd "C-c h t") 'helm-cmd-t)
(global-set-key (kbd "C-c h g g") 'helm-git-grep)
(global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h a") 'helm-org-agenda-files-headings)
(global-set-key (kbd "C-c h r") 'helm-recentf)

;; Yasnippet
;; readme: http://www.howardism.org/Technical/Emacs/templates-tutorial.html
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode)
  :init (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all t))

;; Company everywhere
(use-package company
  :init (global-company-mode)
  :config
  (global-set-key [C-tab] 'company-clang)
  (setq company-auto-complete nil
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5))

;; Move Text
(use-package move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; C-MODE
(setq-default c-basic-offset 4
              c-default-style "bsd")

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package whitespace
  :diminish global-whitespace-mode
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face tabs trailing)))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom
  (column-number-mode t)
  (global-visual-line-mode t))

(use-package startup
  :ensure nil
  :custom
  (initial-scratch-message  ";;|-----------|\n;;| This      |\n;;| is        |\n;;| not       |\n;;| a         |\n;;| Scratch   |\n;;|-----------|\n;;(\\__/) ||\n;;(•ㅅ•) ||\n;;/ 　 づ\n\n"))

(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;; Upcase and downcase
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

;; it's magit bro!
(use-package magit)

(use-package markdown-mode
  :mode (("\\.m[ark]*d[own]*" . gfm-mode))
  :custom
  (markdown-indent-on-enter nil))
(use-package markdown-mode+)

(use-package olivetti
  :ensure t
  :config
  (setq-default
   olivetti-hide-mode-line t
   olivetti-body-width 80))

(use-package fountain-mode
  :ensure t
  :config
  (set-face-attribute 'fountain-scene-heading nil :foreground "#202226" :weight 'bold)
  (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))
  (add-hook 'fountain-mode-hook (lambda () (turn-on-olivetti-mode)))
  (defun export-to-pdf ()
    (shell-command-to-string (format "afterwriting --config afterwriting-config.json --source %s --pdf --overwrite" buffer-file-name)))
  (add-hook 'after-save-hook #'export-to-pdf))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb))
  :custom (org-agenda-files (list "~/.emacs.d/org")))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DROP(x!)"))
      org-log-into-drawer t)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats
      '("<%a %d %b %Y" . "<%a %d %b %Y %H:%M>"))

(use-package smart-comment
  :bind ("M-;" . smart-comment))

;; --- Useful keybindings
(unbind-key "C-z")
(bind-key "C-z" 'undo)
(bind-key "C-q" 'kill-this-buffer)

;; Set the cursor as a box
(set-cursor-color "#ffff00")

;; use ibuffer instead of buffer
(bind-key "C-x C-b" 'ibuffer)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; switch windows in a fast manner way
(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame.")
  :bind
  ("M-o" . ace-window))

;;find and replace expressions in an emacs buffer
(global-set-key (kbd "M-&") 'query-replace-regexp)
;;show number of matches
(use-package anzu
  :diminish
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  :hook
  (after-init . global-anzu-mode)
  );end anzu

;; find expressions in files in directories using ag
(use-package ag)

;; ;; --- end of .init.el file ---;
