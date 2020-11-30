                                        ;-----------------------;
                                        ;--- Emacs Bootstrap ---;
                                        ;-----------------------;

;; ---  Personal information
(setq user-full-name "Xavier Belanche"
      user-mail-address "xbelanch@protonmail.com"
      calendar-latitude 41.499959
      calendar-longitude 2.181137
      calendar-location-name "Barcelona, Spain")

                                        ;-----------------;
                                        ;--- Constants ---;
                                        ;-----------------;

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(when sys/win32p
  (let ((base-system "cygwin64")) ;; msys64
    (setenv "PATH"
            (concat (getenv "PATH")
                    (format ";C:\\%s\\usr\\local\\bin" base-system)
                    (format ";C:\\%s\\opt\\bin" base-system)
                    (format ";C:\\%s\\usr\\bin" base-system)
                    (format ";C:\\%s\\bin" base-system)
                    (format ";C:\\%s\\mingw64\\bin" base-system)))
    (setq shell-file-name "C:/cygwin64/bin/bash")))


;; --- GC threshold to 1GB
;; What that does it means? I dunno, but that's the Mando way
;; Make startup faster by reducing the frequency of garbage
;; collection.
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

(defvar local-dir (concat emacs-dir "local/")
  "Root directory for local Emacs files.")

(defvar cache-dir (concat local-dir "cache/")
  "Where volatile files are storaged.")

;; --- Set directory to add custom elisp code or installing packages
;; http://ergoemacs.org/emacs/emacs_installing_packages.html
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; --- Handle with backup emacs files
;; Source: https://stackoverflow.com/questions/2680389/how-to-remove-all-files-ending-with-made-by-emacs
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))


;; --- Add Melpa package list

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

;;; Now we are ready to initialize package system.
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


(use-package auto-package-update
  :custom
  (auto-package-update-last-update-day-path (concat cache-dir ".last-package-update-day"))
  (auto-package-update-delete-old-versions t))


;; https://github.com/emacsmirror/diminish
;; This package implements hiding or abbreviation of the mode line displays (lighters) of minor-modes.
(use-package diminish)

                                        ;-------------------------------------------;
                                        ;---  UTF-8 as the default coding system ---;
                                        ;-------------------------------------------;

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
  (set-clipboard-coding-system 'utf-16le-dos)
  ;; (set-language-environment "Catalan")
  (setq locale-coding-system   'utf-8)
  (setq-default buffer-file-coding-system 'utf-8))



                                        ;----------------;
                                        ;--- Defaults ---;
                                        ;----------------;
(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 ring-bell-function 'ignore                       ; Disable the annoying bell system
 display-time-default-load-average nil            ; Don't display load average
 fill-column 80                                   ; Set width for automatic line breaks
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Prefers spaces over tabs
 inhibit-startup-screen t                         ; Disable start-up screen
;; initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 kill-ring-max 128                                ; Maximum length of kill ring
 load-prefer-newer t                              ; Prefers the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 read-process-output-max (* 1024 1024)            ; Increase the amount of data reads from the process
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 4                                      ; Set width for tabs
 use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
 vc-follow-symlinks t                             ; Always follow the symlinks
 global-auto-revert-mode t                        ; Auto-refresh all buffers when files have changed on disk
 view-read-only t)                                ; Always open read-only buffers in view-mode
(cd "~/")                                         ; Move to the user directory
(column-number-mode 1)                            ; Show the column number
(display-time-mode 1)                             ; Enable time in the mode-line
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode)                             ; Hightlight current line
;(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom
  (column-number-mode t)
  (global-visual-line-mode t))

(use-package startup
  :ensure nil
  :custom
  (initial-scratch-message nil))

;; @TODO: Need to explore recent files:
;; src: https://gitlab.com/JesusMtnez/emacs.d/-/blob/master/core/00-core.el
;; src: https://github.com/rememberYou/.emacs.d/blob/master/config.org#ivy

(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;;; --- Key discoverability
;;; If you type a prefix key (such as `C-x r`) and wait some time then
;;; display window with keys that can follow.
(use-package which-key
  :hook (after-init . which-key-mode))

(use-package restart-emacs)

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
  :defer 3
  :bind (("M-o" . ace-window))
  :custom
  (aw-dispatch-always t "Issue read-char even for one window")
  (ace-window-display-mode t)
  :config
  (push " *NeoTree*" aw-ignored-buffers)
  (push "*which-key*" aw-ignored-buffers))

(use-package windmove
  :demand
  ;; :bind
  ;; (("M-S-<down>" . windmove-down)
  ;;  ("M-S-<up>" . windmove-up)
  ;;  ("M-S-<left>" . windmove-left)
  ;;  ("M-S-<right>" . windmove-right))
  :config
  (windmove-default-keybindings))

                                        ;-----------------------;
                                        ;--- Fonts and Icons ---;
                                        ;-----------------------;

(add-hook 'after-init-hook
          (lambda ()
            (when (member "Fantasque Sans Mono" (font-family-list))
              (set-face-attribute 'default nil :font "Fantasque Sans Mono")
              (set-face-attribute 'default nil :height 140)
              (set-face-attribute 'default nil :weight 'normal))))


(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-faicon)
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))


                                        ;------------------;
                                        ;--- Doom theme ---;
                                        ;------------------;
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t))

                                        ;----------------------------;
                                        ;--- Visual helpers bells ---;
                                        ;----------------------------;

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

(use-package whitespace
  :diminish global-whitespace-mode
  :hook (after-init . global-whitespace-mode)
  :custom
  (whitespace-style '(face tabs trailing)))



                                        ;-----------------;
                                        ;--- Dashboard ---;
                                        ;-----------------;

(use-package dashboard
  :demand
  :if (< (length command-line-args) 2)
  :bind (:map dashboard-mode-map
              ("U" . auto-package-update-now)
              ("R" . restart-emacs)
              ("K" . kill-emacs))
  :custom
  (dashboard-startup-banner (concat user-emacs-directory "assets/rotter_lyud.png"))
  (dashboard-banner-logo-title "Rotter y Lyud saliendo del necrots!")
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info nil)
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(
     ((,(and (display-graphic-p)
             (all-the-icons-faicon "github" :height 1.2 :v-adjust -0.1))
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
                          (recents . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))


                                        ;--------------------;
                                        ;--- Productivity ---;
                                        ;--------------------;

(require 'bind-key)

;;--- At the moment I decided to comment that
;; @TODO: Need a more testing
;; (use-package recentf
;;   :ensure nil
;;   :config
;;   (add-to-list 'recentf-exclude (format "%selpa.*" emacs-dir))
;;   :custom
;;   (recentf-save-file (concat cache-dir "recentf")))

                                        ;--- Counsel ---;

(use-package counsel
  :after ivy
  :config (counsel-mode))

;; Better performance on Windows
(when sys/win32p
  (setq ivy-dynamic-exhibit-delay-ms 200))

(setq swiper-action-recenter t)

                                        ;--- Ivy ---;

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B"   . ivy-switch-buffer-other-window))
  :commands ivy-mode
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

;; More friendly display transformer for Ivy
(use-package ivy-rich
    :after ivy)

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))


;; Common actions for counsel-ag, counsel-fzf, and counsel-recentf
(defun my-counsel-fzf-in-default-dir (_arg)
  "Search the current directory with fzf."
  (counsel-fzf ivy-text default-directory))
(defun my-counsel-fzf-in-dir (_arg)
  "Search again with new root directory."
  (counsel-fzf ivy-text
               (read-directory-name
                (concat (car (split-string counsel-fzf-cmd))
                        " in directory: "))))
(defun my-counsel-ag-in-dir (_arg)
  "Search again with new root directory."
  (let ((current-prefix-arg '(4)))
    (counsel-ag ivy-text nil ""))) ;; also disable extra-ag-args


                                        ;---  Smex ---;
(use-package smex
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

                                        ;--- Swiper ---;
(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

                                        ;--- Projectile ---;

(use-package projectile
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-command-map
              ("a" . projectile-add-known-project))
  :custom
  (projectile-enable-caching nil)
  (projectile-indexing-method 'hybrid)
  (projectile-completion-system 'ivy)
  (projectile-cache-file (concat cache-dir "projectile.cache"))
  (projectile-known-projects-file (concat cache-dir "projectile.projects"))
  (projectile-project-root-files '(".git" ".project" "setup.py" "build.sbt" "pom.xml"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".class"))
  (projectile-globally-ignored-files '(".DS_Store" "Icon")))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))


                                        ;--- Neotree ---;

(use-package neotree
  ;; :functions (neotree-resize-window neotree-project-dir)
  :commands neotree-project-dir
  ;; :hook ((neo-enter . neotree-resize-window))
  :bind ("<f9>" . 'neotree-project-dir)
  :config

  ;; This is really annoying!
  ;; (defun neotree-resize-window (&rest _args)
  ;;   "Resize neotree window."
  ;;   (neo-global--when-window
  ;;    (let ((fit-window-to-buffer-horizontally t))
  ;;      (neo-buffer--unlock-width)
  ;;      (fit-window-to-buffer)
  ;;      (neo-buffer--lock-width))))

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
  (neo-window-width 24)
  (neo-create-file-auto-open t)
  (neo-show-updir-line nil)
  (neo-mode-line-type 'neotree)
  (neo-smart-open t)
  (neo-autorefresh t)
  (neo-auto-indent-point t)
  (neo-show-hidden-files t)
  (neo-window-fixed-size nil))

                                        ;--------------------------------;
                                        ;--- GIT - MAGIT - IT'S MAGIC ---;
                                        ;--------------------------------;

(use-package git-commit
  :hook (after-init . global-git-commit-mode))

;; In addition to that, I like to see the lines that are being modified in the file while it is being edited.
(use-package git-gutter
  :hook ((prog-mode text-mode) . git-gutter-mode)
  :diminish git-gutter-mode
  :custom
  (git-gutter:hide-gutter t)
  (git-gutter:update-interval 0.5)
  (git-gutter:verbosity 0))

(use-package gitignore-mode)

(use-package git-link)

(use-package git-messenger)

(use-package git-timemachine)

(use-package magit
  :bind ("C-x g" . magit-status))

                                        ;--------------------------------------;
                                        ;--- Markdown / Olivetti / Fountain ---;
                                        ;--------------------------------------;

(use-package markdown-mode
  :mode (("\\.m[ark]*d[own]*" . gfm-mode))
  :custom
  (markdown-indent-on-enter nil))

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

;; Duplicate this file
;; source: https://emacs.stackexchange.com/questions/60661/how-to-duplicate-a-file-in-dired
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

;-----------;
;--- ORG ---;
;-----------;

(use-package org
  :custom
  (org-log-done t))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DROP(x!)"))
      org-log-into-drawer t)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats
  '("<%a %d %b %Y" . "<%a %d %b %Y %H:%M>"))

;; Provides notifications for scheduled or deadlined agenda entries.
;; src: https://github.com/spegoraro/org-alert
;; src: https://github.com/gkowzan/alert-toast
(require 'org-alert)
(require 'alert-toast)
(setq alert-default-style 'libnotify)
(when sys/win32p
  (setq alert-default-style 'toast))


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
;; Font: https://www.gnu.org/software/emacs/manual/html_node/elisp/Cursor-Parameters.html
(set-cursor-color "#ffff00")

                                        ;-------------------;
                                        ;--- Programming ---;
                                        ;-------------------;

                                        ;--- Web development ---;

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

;; @TODO: Need to review that:
;; src: https://github.com/rememberYou/.emacs.d/blob/master/config.org#javascript
;; For my JavaScript configuration, I took my sources from the Nicolas Pettons blog which I found very well explained.
;;Setting up Emacs for JavaScript (part #1) Setting up Emacs for JavaScript (part #2)
;;I like to use prettier to get my TypeScript code clean. To use it, dont forget to install it with your package manager.

;; By default, GNU Emacs uses js-mode as major mode for JavaScript buffers and I prefer use js2-mode instead because of his abilities to parses buffers and builds an AST for things like syntax highlighting.

(use-package js2-mode
  :init
  (setq js-indent-level 2)
  :mode (("\\.js" . js2-mode)
         ("\\.sjs" . js2-mode))
  :config
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode) ;; Better imenu
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package xref-js2)

                                        ;--- JSON ---;

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\manifest.webapp\\'" . json-mode )
         ("\\.tern-project\\'" . json-mode)))

(use-package jsonnet-mode)

                                        ;--- YAML ---;

(use-package yaml-mode
  ; repo: https://github.com/yoshiki/yaml-mode
  :mode (("\\.yml$" .  yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :bind (("C-m" . newline-and-indent)))


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


;; Source:
;; https://github.com/Fanael/persistent-scratch
(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

                                        ;-----------------------------------;
                                        ;--- Path filename to clipboard  ---;
                                        ;-----------------------------------;

(defun copy-file-path-on-clipboard ()
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

;; --- end of .init.el file ---;
