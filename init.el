;;; -*- lexical-binding: t; -*-

;; This is me!
(setq user-full-name "Xavier Belanche"
      user-mail-address "xbelanch@protonmail.com"
      calendar-latitude 41.499959
      calendar-longitude 2.181137
      calendar-location-name "Barcelona, Spain")

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

;; Constants and globals variables
(defconst sys/gui
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst sys/win32
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linux
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/mac
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/root
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst rg
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst fzf
  (executable-find "fzf")
  "Do we have fzf?")

(defconst ruby
  (executable-find "ruby")
  "Do we have ruby")

(defconst gcc
  (executable-find "gcc")
  "Do we have gcc?")

(defconst git
  (executable-find "git")
  "Do we have git?")

(defconst pandoc
  (executable-find "pandoc")
  "Do we have Pandoc?")

(defconst xelatex
  (executable-find "xelatex")
  "Do we have xelatex?")

(defvar version "0.0.1"
  "Current version of my personal emacs dot files.")

(defvar homepage "https://github.com/xbelanch/personal-emacs-dot-files"
  "Project homepage.")

(defvar emacs-dir (expand-file-name user-emacs-directory)
  "The path to this emacs.d directory.")

(defvar core-dir  "core/"
  "Where essential files are stored.")

(defvar local-dir (concat emacs-dir "local/")
  "Root directory for local Emacs files.")

(defvar cache-dir (concat local-dir "cache/")
  "Where volatile files are storaged.")

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
  :ensure t
  :init
  (gcmh-mode 1))

;; Allow some things that emacs would otherwise confirm.
(put 'eval-expression  'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column  'disabled nil)

;; No flashing, please.
(setq visible-bell nil)

;; Set language environment to Catalan?
(set-language-environment "Catalan")

;; Also, I’ve seen the startup message.
(setq inhibit-startup-message t)

;; Show me errors
;; set true if you want to display whole error stuff
(setq debug-on-error nil)

;; Show me more log messages
(setq message-log-max 500)

;; WTF? If I don't define this, I get weird warnings when byte compiling
(setq warning-suppress-types nil)

;; Don't display load average
(setq  display-time-default-load-average nil)

;; Hide the cursor in inactive windows
(setq cursor-in-non-selected-windows t)

;; Set width for automatic line breaks
(setq fill-column 80)

;; Set width for tabs
(setq tab-width 4)

;; Avoid the :ensure keyword for each package
(setq use-package-always-ensure t)

;; Always follow the symlinks
(setq vc-follow-symlinks t)

;; Electric pair mode for completing parenthesis or brackets
(setq electric-pair-preserve-balance nil)

;; Auto-refresh all buffers when files have changed on disk
(setq global-auto-revert-mode t)

;; Always open read-only buffers in
(setq view-read-only t)

;; How often to do auto-saving, in terms of number of input events.
(setq auto-save-interval  2048)

;; How many characters back to search to find the matching opening delimiter.
(setq blink-matching-paren-distance 100000)

(setq-default fill-column 80)

(setq sentence-end-double-space nil)

(setq-default indicate-empty-lines t)

(setq-default indent-tabs-mode nil)

;; Shift + Enter for inser new line and jump to it
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

(cd "~/")                                         ; Move to the user directory
(column-number-mode 1)                            ; Show the column number
(display-time-mode 1)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(show-paren-mode 1)                               ; Show the parent

;; MULtilingual enhancement to Emacs
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

                                        ;--------------------;
                                        ;--- GUI / Window ---;
                                        ;--------------------;

;; Most of the time, when I open a new window with C-x 2 or C-x 3 it is to switch directly to it and perform an action. By default, GNU Emacs does not give focus to the new window created. I have no idea why this is not the default behavior. But let's refine these keys:
(use-package window
  :ensure nil
  :bind (("C-x 3" . hsplit-last-buffer)
         ("C-x 2" . vsplit-last-buffer))
  :preface
  (defun hsplit-last-buffer ()
    "Gives the focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Gives the focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

;; Disabling GUI defaults
(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :bind ("C-x C-k" . kill-this-buffer)
  :config (menu-bar-mode -1))

(use-package tool-bar
  :ensure nil
  :config (tool-bar-mode -1))

(use-package tooltip
  :ensure nil
  :config (tooltip-mode -1))

(if window-system
    (progn
      ;; Maximize Emacs at startup
      ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (setq use-default-font-for-symbols nil)
      (setq inhibit-compacting-font-caches t)))

(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Use home row for selecting.")
  (aw-scope 'frame "Highlight only current frame.")
  :bind
  ("M-o" . ace-window))

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

                                        ;-----------------------;
                                        ;--- Fonts and Icons ---;
                                        ;-----------------------;

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


                                        ;------------------;
                                        ;--- Doom theme ---;
                                        ;------------------;
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/gruber-darker-theme/")

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  ;; (load-theme 'doom-solarized-dark t))
  (load-theme 'gruber-darker t))

                                        ;----------------------------;
                                        ;--- Visual helpers bells ---;
                                        ;----------------------------;

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type (quote relative)))

(use-package whitespace
  :diminish global-whitespace-mode
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face tabs trailing)))

                                        ;-------------;
                                        ;--- Mixed ---;
                                        ;-------------;

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

;; Key discoverability
;;; If you type a prefix key (such as `C-x r`) and wait some time then
;;; display window with keys that can follow.
(use-package which-key
  :hook (after-init . which-key-mode))

(use-package restart-emacs)


                                        ;-------------;
                                        ;--- Dired ---;
                                        ;-------------;

(use-package dired
  :ensure nil
  :delight "Dired "
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  (dired-ls-F-marks-symlinks nil)
  (dired-recursive-copies 'always))

(use-package dired-narrow
  :bind (("C-c C-n" . dired-narrow)
         ("C-c C-f" . dired-narrow-fuzzy)
         ("C-c C-r" . dired-narrow-regexp)))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("<backtab>" . dired-subtree-cycle)
              ("<tab>" . dired-subtree-toggle)))

(use-package diredfl
  :init (diredfl-global-mode 1))

(use-package dired-git-info
  :ensure t
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

(use-package dired-recent
  :ensure t
  :bind
  (:map
   dired-recent-mode-map ("C-x C-d" . nil))
  :config
  (dired-recent-mode 1))

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


                                        ;-------------------------------;
                                        ;--- Text manipulation extras---;
                                        ;-------------------------------;

(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

;; Upcase and downcase
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

;; Anzu
;; Source: https://github.com/syohex/emacs-anzu
(use-package anzu
  :ensure t
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

;; Duplicate things
(use-package duplicate-thing
  :ensure t
  :bind ("C-c C-d" . duplicate-thing))

;; avy-jump
(use-package avy
  :ensure t
  :bind (("M-SPC" . 'avy-goto-char-timer)
         ("M-g a" . 'avy-goto-line)))

                                        ;------------------;
                                        ;--- IT'S MAGIT ---;
                                        ;------------------;

(use-package git-commit
  :hook (after-init . global-git-commit-mode))

;; The mode-line information isn’t always up-to-date
;; src: https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html
;; src: https://emacs.stackexchange.com/questions/48090/mode-line-not-updated-after-checking-out-new-branch-using-magit

;; (setq vc-handled-backends nil)

(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;; Solution: https://www.reddit.com/r/emacs/comments/10rdl2/globalautorevertmode_1/
(global-set-key (kbd "<f5>") 'my/revert-buffer-no-confirm)

;; In addition to that, I like to see the lines that are being modified in the file while it is being edited.
(use-package git-gutter
  :hook ((prog-mode text-mode) . git-gutter-mode))
  ;; :diminish git-gutter-mode
  ;; :custom
  ;; (git-gutter:hide-gutter t)
  ;; (git-gutter:update-interval 2.0)
  ;; (git-gutter:verbosity 0))

(use-package gitignore-mode)

(use-package git-link)

(use-package git-messenger)

(use-package git-timemachine)

(use-package magit
  :bind ("C-x g" . magit-status))

;;                                         ;--------------------;
;;                                         ;--- Productivity ---;
;;                                         ;--------------------;

(use-package multiple-cursors
    :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package recentf
  :init
  (setq recentf-max-menu-items 9)
  :config
  (recentf-mode 1))

;; Disk usage
;;; disk-usage is a file system analyzer: it offers a tabulated view of file listings sorted by size. Directory sizes are computed recursively.
(use-package disk-usage
  :commands (disk-usage))

;; https://github.com/emacsmirror/diminish
;; This package implements hiding or abbreviation of the mode line displays (lighters) of minor-modes.
(use-package diminish)

(use-package deadgrep)

(use-package bind-key)

(use-package charmap)

;; Ido
(use-package smex)
(use-package ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Helm
(use-package helm)
(use-package helm-cmd-t)
(use-package helm-git-grep)
(use-package helm-ls-git)
(use-package helm-ag)
(use-package helm-projectile)

(setq helm-split-window-in-side-p t)

(add-to-list 'display-buffer-alist
             '("\\`\\*helm.*\\*\\'"
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.33)))

(setq helm-swoop-split-with-multiple-windows nil
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-split-window-function 'helm-default-display-buffer)

(setq helm-ff-transformer-show-only-basename nil)
(setq helm-ff-skip-boring-files t)

(global-set-key (kbd "C-x f") 'helm-for-files)
(global-set-key (kbd "C-c h f") 'helm-find-files)
(global-set-key (kbd "C-c h t") 'helm-cmd-t)
(global-set-key (kbd "C-c h g g") 'helm-git-grep)
(global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
(global-set-key (kbd "C-c h r") 'helm-recentf)
(global-set-key (kbd "C-c h a") 'helm-ag)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config (setq-default projectile-enable-caching nil))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

                                        ;--------------------------------------;
                                        ;--- Markdown / Olivetti / Fountain ---;
                                        ;--------------------------------------;

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


                                        ;-----------;
                                        ;--- ORG ---;
                                        ;-----------;

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

                                        ;----------------------;
                                        ; --- Miscellaneous ---;
                                        ;----------------------;

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

;;                                         ;-------------------;
;;                                         ;--- Programming ---;
;;                                         ;-------------------;



(setq compile-command "make -B")
(setq compilation-scroll-output t)
(setq compilation-window-height 20)
(set-frame-parameter nil 'unsplittable t) ;; prevent window split in a single frame

(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

(use-package company
  :init (global-company-mode)
  :config
  (global-set-key [C-tab] 'company-clang)
  (setq company-auto-complete nil
        company-tooltip-flip-when-above t
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-idle-delay 0.5))

;; readme: http://www.howardism.org/Technical/Emacs/templates-tutorial.html
(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode)
  :init (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-reload-all t))

;; cc-mode
(use-package cc-mode
  :commands (cc-mode)
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'inextern-lang 0)
              (setq-local c-default-style "K&R")
              (setq-local indent-tabs-mode nil)
              (setq-local tab-width 4)
              (setq-local c-basic-offset 4)))
  (list c-mode-map c++-mode-map))

;; gdb
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; web-mode
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css?\\'")
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  :custom-face
  (web-mode-html-tag-bracket-face ((nil (:foreground "Snow3"))))
  (web-mode-current-element-highlight-face ((nil (:background "#073642")))))

(use-package emmet-mode
  :after (web-mode)
  :hook (web-mode . emmet-mode))

(use-package js2-mode
  :init
  (setq js-indent-level 2)
  :mode (("\\.js" . js2-mode)
         ("\\.sjs" . js2-mode))
  :config
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)) ;; Better imenu
(use-package xref-js2)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(setq xref-js2-search-program 'ag)
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\manifest.webapp\\'" . json-mode )
         ("\\.tern-project\\'" . json-mode)))
(use-package jsonnet-mode)

(use-package web)

(use-package yaml-mode
  ; repo: https://github.com/yoshiki/yaml-mode
  :mode (("\\.yml$" .  yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :bind (("C-m" . newline-and-indent)))

;; C# - Unity
;; https://gist.github.com/kurogomapurin/42386006f9aaa7187840
;; https://joshwolfe.ca/post/emacs-for-csharp/
;; Compile https://stackoverflow.com/questions/6319274/how-do-i-run-msbuild-from-the-command-line-using-windows-sdk-7-1

;; Finalment, la millor opció és fer servir el omnisharp-server-install
;; en el moment de fer l'start ens demanarà el root (on tinguem l'sln.)
(use-package csharp-mode)

(if (eq system-type 'windows-nt)
  (use-package omnisharp
    :after company
    :config
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)))


;;; Programming languages packages that don't require any configuration
(use-package glsl-mode)
(use-package lua-mode)
(use-package graphviz-dot-mode)
(use-package haxe-mode)

;;                                         ;--------------------;
;;                                         ;--- Localization ---;
;;                                         ;--------------------;

;; Rellotge format 24 hores
(setq display-time-day-and-date t
display-time-24hr-format t)
(display-time)
;
;; Posar en català el calendari
;; Font: https://www.emacswiki.org/emacs/CalendarLocalization#toc4
(setq european-calendar-style 't)
(setq
    calendar-week-start-day 1
    calendar-day-name-array ["dg" "dll" "dm" "dx" "dj" "dv" "ds"]
    calendar-month-name-array ["gener" "febrer" "març" "abril" "maig" "juny" "juliol" "agost" "setembre" "octubre" "novembre" "desembre"]
   )

                                        ;-----------------------------------;
                                        ;--- Insert time stamp functions ---;
                                        ;-----------------------------------;

;; Source:
;; https://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "(%A %d/%m/%Y %-H:%M)")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

                                         ;-------------------------;
                                         ;--- Persisten scratch ---;
                                         ;-------------------------;

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

                                        ;--------------------------------------;
                                        ;--- Functions I taken from nowhere ---;
                                        ;--------------------------------------;
(defun my/buffer-file-name ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun my/put-buffer-name-on-clipboard ()
  "Put the current buffer name on the clipboard"
  (interactive)
  (kill-new (buffer-name))
  (message (buffer-name)))

;;; Taken from here:
;;; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun my/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (my/buffer-file-name)))
    (when filename
      (kill-new filename)
      (message filename))))

(defun my/put-file-path-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun my/start-simple-http-server ()
  (interactive)
  (shell-command "http-server.cmd -c-1 -p 8010 &"
                 "*Simple npm HTTP Server*"))

(global-set-key (kbd "C-x p s") 'my/start-simple-http-server)

;; Revert all open buffers (and ignore errors)
(defun my/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))



;;; Stolen from http://ergoemacs.org/emacs/emacs_unfill-paragraph.html
(defun my/unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))

(global-set-key (kbd "C-c M-q") 'my/unfill-paragraph)

(defconst my/frame-transparency 65)

(defun my/toggle-transparency ()
  (interactive)
  (let ((frame-alpha (frame-parameter nil 'alpha)))
    (if (or (not frame-alpha)
            (= (cadr frame-alpha) 100))
        (set-frame-parameter nil 'alpha
                             `(,my/frame-transparency
                               ,my/frame-transparency))
      (set-frame-parameter nil 'alpha '(100 100)))))

;;                                         ;-----------------;
;;                                         ;--- Dashboard ---;
;;                                         ;-----------------;

;; Uncomment next if you miss something nice at the startup

;; (use-package dashboard
;;   :demand
;;   :if (< (length command-line-args) 2)
;;   :bind (:map dashboard-mode-map
;;               ("U" . auto-package-update-now)
;;               ("R" . restart-emacs)
;;               ("K" . kill-emacs))
;;   :custom
;;   (dashboard-startup-banner (concat user-emacs-directory "rotter_lyud.png"))
;;   (dashboard-banner-logo-title "Rotter y Lyud saliendo del necrots!")
;;   (dashboard-set-heading-icons t)
;;   (dashboard-set-file-icons t)
;;   (dashboard-set-init-info nil)
;;   (dashboard-set-navigator t)
;;   (dashboard-navigator-buttons
;;    `(
;;      ((,(and (display-graphic-p)
;;              (all-the-icons-faicon "github" :height 1.2 :v-adjust -0.1))
;;        "Homepage"
;;        "Browse Homepage"
;;        (lambda (&rest _) (browse-url homepage)))
;;       (,(and (display-graphic-p)
;;              (all-the-icons-material "update" :height 1.2 :v-adjust -0.24))
;;        "Update"
;;        "Update emacs"
;;        (lambda (&rest _) (auto-package-update-now)))
;;       (,(and (display-graphic-p)
;;              (all-the-icons-material "autorenew" :height 1.2 :v-adjust -0.15))
;;        "Restart"
;;        "Restar emacs"
;;        (lambda (&rest _) (restart-emacs))))))
;;   (dashboard-set-footer t)
;;   (dashboard-footer-messages (list (format "Powered by Rotter and Lyud, %s" (format-time-string "%Y"))))
;;   (dashboard-footer-icon (cond ((display-graphic-p)
;;                                 (all-the-icons-faicon "code" :height 1.5 :v-adjust -0.1 :face 'error))
;;                                (t (propertize ">" 'face 'font-lock-doc-face))))
;;   :config
;;   (defun dashboard-load-packages (list-size)
;;     (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 2) ? )
;;             (format "[%d packages loaded in %s]" (length package-activated-list) (emacs-init-time))))

;;   (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages))

;;   (setq dashboard-items '((packages)
;;                           (projects . 10)
;;                           (recents . 5)
;;                           (agenda . 5)))
;;   (dashboard-setup-startup-hook))

;; --- end of .init.el file ---;
