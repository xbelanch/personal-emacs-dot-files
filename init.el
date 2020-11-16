                                        ;-----------------------;
                                        ;--- Emacs Bootstrap ---;
                                        ;-----------------------;

;; ---  Personal information
(setq user-full-name "Xavier Belanche"
      user-mail-address "xbelanch@protonmail.com"
      calendar-latitude 41.499959
      calendar-longitude 2.181137
      calendar-location-name "Barcelona, Spain")


;; --- GC threshold to 1GB
;; What that does it means? I dunno, but that's the Mando way
(setq gc-cons-threshold 1000000000
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

;; --- Global variables definition
(defvar version "0.1.0"
  "Current version of my personal emacs dot files.")

(defvar homepage "https://github.com/xbelanch/personal-emacs-dot-files"
  "Project homepage.")

(defvar emacs-dir (expand-file-name user-emacs-directory)
  "The path to this emacs.d directory.")

(defvar core-dir  "core/"
  "Where essential files are stored.")

(defvar modules-dir "modules/"
  "Where configuration modules are stored.")

(defvar local-dir (concat emacs-dir "local/")
  "Root directory for local Emacs files.")

(defvar cache-dir (concat local-dir "cache/")
  "Where volatile files are storaged.")


(require 'package)
(setq package-enable-at-startup nil)


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


(package-initialize)

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-compute-statistics t)

(require 'bind-key)
(require 'cl-lib)

(use-package diminish)

;;;; 00-core.el --- the core of the configuration

;; UTF-8 as the default coding system
(use-package mule
  :ensure nil
  :config
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system        'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (setq locale-coding-system   'utf-8)
  (setq-default buffer-file-coding-system 'utf-8))

(use-package emacs
  :ensure nil
  :bind ("C-c C-c" . comment-or-uncomment-region-or-line)
  :custom
  (read-process-output-max (* 1024 1024))
  (cursor-type 'bar)
  (make-backup-files nil)
  (c-basic-offset 2)
  (tab-width 2)
  (ring-bell-function 'ignore)
  (tab-always-indent nil)
  (indent-tabs-mode nil)
  (show-paren-mode t)
  (electric-pair-mode t)
  (delete-selection-mode t)
  (global-auto-revert-mode t)
  (custom-file null-device "Do not store customizations")
  ; Smooth scrolling
  (redisplay-dont-pause t)
  (scroll-margin 5)
  (scroll-step 1)
  (scroll-conservatively 10000)
  (scroll-preserve-screen-position t)
  :config
  (put 'downcase-region 'disabled nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (load (concat user-emacs-directory "localrc.el") 'noerror))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom
  (column-number-mode t)
  (global-visual-line-mode t))

(use-package startup
  :ensure nil
  :custom
  (initial-scratch-message nil)
  (initial-major-mode 'org-mode))

(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude (format "%selpa.*" emacs-dir))
  :custom
  (recentf-save-file (concat cache-dir "recentf")))

(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package restart-emacs)

;;; 00-core.el ends here


;;; 20-core-ui.el --- User Insterface core

(use-package all-the-icons
  :commands (all-the-icons-faicon)
  :config
  (setq inhibit-compacting-font-caches t))

(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t))

(use-package dashboard
  :demand
  :if (< (length command-line-args) 2)
  :bind (:map dashboard-mode-map
              ("U" . auto-package-update-now)
              ("R" . restart-emacs)
              ("K" . kill-emacs))
  :custom
  (dashboard-startup-banner (concat user-emacs-directory "assets/emacs-vscode.png"))
  (dashboard-banner-logo-title "The One True Editor, Emacs")
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info nil)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(
     ((,(and (display-graphic-p)
             (all-the-icons-faicon "gitlab" :height 1.2 :v-adjust -0.1))
       "Homepage"
       "Browse Homepage"
       (lambda (&rest _) (browse-url homepage)))
      (,(and (display-graphic-p)
             (all-the-icons-material "update" :height 1.2 :v-adjust -0.24))
       "Update"
       "Update emacs"
       (lambda (&rest _) (auto-package-update-now)))
      (,(and (display-graphic-p)
             (all-the-icons-material "autorenew" :height 1.2 :v-adjust -0.15))
       "Restart"
       "Restar emacs"
       (lambda (&rest _) (restart-emacs))))))
  (dashboard-set-footer t)
  (dashboard-footer-messages (list (format "Powered by Rotter and Lyud, %s" (format-time-string "%Y"))))
  (dashboard-footer-icon (cond ((display-graphic-p)
                                (all-the-icons-faicon "code" :height 1.5 :v-adjust -0.1 :face 'error))
                               (t (propertize ">" 'face 'font-lock-doc-face))))
  :config
  (defun dashboard-load-packages (list-size)
    (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 2) ? )
            (format "[%d packages loaded in %s]" (length package-activated-list) (emacs-init-time))))

  (add-to-list 'dashboard-item-generators '(packages . dashboard-load-packages))

  (setq dashboard-items '((packages)
                          (projects . 10)
                          (recents . 10)))
  (dashboard-setup-startup-hook))


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

(if window-system
    (progn
      ;; Maximize Emacs at startup
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (setq use-default-font-for-symbols nil)
      (setq inhibit-compacting-font-caches t)))


(use-package whitespace
  :diminish global-whitespace-mode
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face tabs trailing)))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(use-package ace-window
  :defer 3
  :bind (("M-o" . ace-window))
  :custom
  (aw-dispatch-always t "Issue read-char even for one window")
  (ace-window-display-mode t)
  :config
  (push " *NeoTree*" aw-ignored-buffers)
  (push "*which-key*" aw-ignored-buffers))

(add-hook 'after-init-hook
          (lambda ()
            (when (member "Fantasque Sans Mono" (font-family-list))
              (set-face-attribute 'default nil :font "Fantasque Sans Mono")
              (set-face-attribute 'default nil :height 130)
              (set-face-attribute 'default nil :weight 'normal))))

(use-package fira-code-mode
  :when (display-graphic-p)
  :hook (after-init . global-fira-code-mode))

;;; 20-core-ui.el ends here


;;; 40-core-helm.el --- helm integration.    -*- lexical-binding: t -*-

(use-package helm
  :hook (after-init . helm-mode)
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-buffers-list)
         ("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         :map helm-map
         ("TAB" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :custom
  (helm-split-window-in-side-p t)
  (helm-move-to-line-cycle-in-source t)
  (helm-ff-search-library-in-sexp t)
  (helm-scroll-amount 8)
  (helm-ff-file-name-history-use-recentf t)
  (helm-autoresize-mode t)
  (helm-autoresize-max-height 20)
  (helm-mode-fuzzy-match t)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t)
  (helm-M-x-fuzzy-match t)
  (helm-mode t)
  :diminish helm-mode)

(use-package helm-swoop
  :after (helm)
  :bind (("C-s" . helm-swoop)
         ("C-r" . helm-swoop)
         :map helm-swoop-map
         ("C-r" . helm-previous-line)
         ("C-s" . helm-next-line))
  :custom
  (helm-swoop-use-fuzzy-match t))

;;; 40-core-helm.el ends here

;;; 50-core-company.el --- company integration
(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :functions (my-company-yasnippet)
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet))
  :custom
  (company-idle-delay 0.1)
  (company-echo-delay 0)
  (company-minimum-prefix-length 0)
  (company-tooltip-limit 12)
  (company-tooltip-align-annotations t)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  :config
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet)))

(unless (version< emacs-version "26.1")
  (use-package company-box
    :diminish
    :after (company)
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-show-single-candidate t)
    (company-box-max-candidates 50)
    (company-box-doc-delay 0.2)
    (company-box-icons-alist 'company-box-icons-all-the-icons)))

;;; 50-core-company.el ends here

;;; 60-core-projects.el --- projects integration

(use-package projectile
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("a" . projectile-add-known-project))
  :custom
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'helm)
  (projectile-cache-file (concat cache-dir "projectile.cache"))
  (projectile-known-projects-file (concat cache-dir "projectile.projects"))
  (projectile-project-root-files '(".git" ".project" "setup.py" "build.sbt" "pom.xml"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class"))
  (projectile-globally-ignored-files '(".DS_Store" "Icon")))

(use-package helm-projectile
  :after (projectile)
  :custom
  (helm-projectile-fuzzy-match t)
  :config
  (helm-projectile-on))

(use-package neotree
  :functions (neotree-resize-window neotree-project-dir)
  :commands neotree-project-dir
  :hook ((neo-enter . neotree-resize-window))
  :bind ("<f8>" . 'neotree-project-dir)
  :config

  (defun neotree-resize-window (&rest _args)
    "Resize neotree window."
    (neo-global--when-window
     (let ((fit-window-to-buffer-horizontally t))
       (neo-buffer--unlock-width)
       (fit-window-to-buffer)
       (neo-buffer--lock-width))))

  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name))))))
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'nerd))
  (neo-window-width 40)
  (neo-create-file-auto-open t)
  (neo-show-updir-line nil)
  (neo-mode-line-type 'neotree)
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-auto-indent-point t)
  (neo-show-hidden-files t)
  (neo-window-fixed-size nil))


;;; module-git.el --- git integration

(use-package helm-gitignore)

(use-package git-commit
  :hook (after-init . global-git-commit-mode))

(use-package gitconfig-mode)

(use-package git-gutter
  :hook ((prog-mode text-mode) . git-gutter-mode)
  :diminish git-gutter-mode
  :custom
  (git-gutter:hide-gutter t)
  (git-gutter:update-interval 0.1)
  (git-gutter:verbosity 0))

(use-package gitignore-mode)

(use-package git-link)

(use-package git-messenger)

(use-package git-timemachine)

(use-package magit
  :bind ("C-x g" . magit-status))

(provide 'module-git)


;;; module-markdown.el --- markdown integration

(use-package markdown-mode
  :mode (("\\.m[ark]*d[own]*" . gfm-mode))
  :custom
  (markdown-indent-on-enter nil))

(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :custom
  (grip-update-after-change nil)
  (grip-preview-use-webkit nil))

(use-package markdown-toc
  :hook (before-save . markdown-toc-refresh-toc)
  :bind (:map markdown-mode-map
              ("C-c i" . markdown-toc-generate-or-refresh-toc)
              ("C-c r" . markdown-toc-refresh-toc)))

(provide 'module-markdown)
;;; module-markdown.el ends here

;;; Olivetti
(use-package olivetti
  :ensure t
  :config
  (setq-default
   olivetti-hide-mode-line t
   olivetti-body-width 80))


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

;;; module-text.el --- Text manipulation extras
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

(provide 'module-text)
;;; module-text.el ends here

;;; module-web.el --- web integration

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

(provide 'module-web)
;;; module-web.el ends here


;;; module-org.el --- org-mode
(use-package org
  :custom
  (org-log-done t))

 (use-package org-bullets
    :ensure t
        :init
        (add-hook 'org-mode-hook (lambda ()
                            (org-bullets-mode 1))))

(setq org-hide-leading-stars t)

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats
  '("<%a %d %b %Y" . "<%a %d %b %Y %H:%M>"))


(provide 'module-org)
;;; module-org.el ends here


;;; module-json.el --- json integration

(use-package json-mode
  :custom
  (js-indent-level 2))

(use-package jsonnet-mode)

(provide 'module-json)
;;; module-json.el ends here

;;; module-yaml.el --- yaml integration

(use-package yaml-mode
  ; repo: https://github.com/yoshiki/yaml-mode
  :mode (("\\.yml$" .  yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :bind (("C-m" . newline-and-indent)))

(use-package gitlab-ci-mode)

(provide 'module-yaml)
;;; module-yaml.el ends here


;;; module-misc.el -- miscellaneous
(use-package smart-comment
  :bind ("M-;" . smart-comment))

;;; --- Useful keybindings
(unbind-key "C-z")
(bind-key "C-z" 'undo)
(bind-key "C-q" 'kill-this-buffer)


;; Set the cursor as a box
;; Font: https://www.gnu.org/software/emacs/manual/html_node/elisp/Cursor-Parameters.html
(setq-default cursor-type 'box)
(set-cursor-color "#ffff00")


;; Colourful Dired
(use-package diredfl
  :ensure t
  :init (diredfl-global-mode 1))


                                        ;--------------------;
                                        ;--- Localization ---;
                                        ;--------------------;

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
