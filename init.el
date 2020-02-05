                                        ;-------------------------;
                                        ;--- Basic Emacs Setup ---;
                                        ;-------------------------;

;; My personal information
(setq user-full-name "Xavier Belanche"
      user-mail-address "xbelanch@protonmail.com"
      calendar-latitude 41.499959
      calendar-longitude 2.181137
      calendar-location-name "Barcelona, Spain")

;; Add MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("MELPA" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("ORG" . "https://orgmode.org/elpa/") t))

(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Set directory to add custom elisp code or installing packages
;; http://ergoemacs.org/emacs/emacs_installing_packages.html
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Making Emacs secure
;; Source: https://github.com/DiegoVicen/my-emacs
;; @NOTE: At this moment it raises an error :-(
(setq tls-checktrust t)
;; This snippet is ready to work in both UNIX-like and Windows OS
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat "python3 -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  ;; (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(defun check-tls-config ()
  "Check for correctness in the TLS configuration for Emacs."
  (interactive)
  (let ((bad-hosts
         (cl-loop for bad
               in `("https://wrong.host.badssl.com/"
                    "https://self-signed.badssl.com/")
               if (condition-case e
                      (url-retrieve
                       bad (lambda (retrieved) t))
                    (error nil))
               collect bad)))
    (if bad-hosts
        (error (format "TLS misconfigured; retrieved %s ok" bad-hosts))
      (url-retrieve "https://badssl.com"
                    (lambda (retrieved) t)))))

;; Disable yes-or-no messages
(defalias 'yes-or-no-p #'y-or-n-p)


;; Default Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; NOTE: This has changed because of this: https://stackoverflow.com/questions/22647517/emacs-encoding-of-pasted-text
;; Maybe it needs to check if you're working on windows or linux...
;; maybe there's a better option to deal with this
(if (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16le-dos)) ;
(if (eq system-type 'gnu/linux)
    (set-clipboard-coding-system 'utf-8)) ;
;; (#1) Using clipboard/copy paste results in chinese looking characters on Linux Mint (search this on Stack Overflow)
;; Now it works: https://stackoverflow.com/questions/9955725/using-clipboard-copy-paste-results-in-chinese-looking-characters-debian-sid
                                        ; (set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8) ; nil
(setq process-coding-system-alist
      (cons '("grep" utf-8 . utf-8) process-coding-system-alist))


;; Remember all the buffers I have open
(desktop-save-mode 1)
(setq history-length 50)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|elpa\/*\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; Disable byte-compile warnings, which I don't care about.
;; Source: http://tsengf.blogspot.ca/2011/06/disable-byte-compile-warning-in-emacs.html
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only
                                  ))
;; Disable startup message
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Disable the warning when killing a buffer with process
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	kill-buffer-query-functions))

;; Disable the bell
(setq ring-bell-function 'ignore)

;; Disable abbreviations prompt
(setq save-abbrevs 'silent)

;; Disable tabs forever and fix indentation offset
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;; Fix scroll
(setq scroll-step            1
      scroll-conservatively  10000
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Real auto-save feature
(use-package real-auto-save
  :ensure t
  :demand t
  :config (setq real-auto-save-interval 10)
  :hook (prog-mode . real-auto-save-mode))

;; Insert new line without breaking
(defun insert-new-line-below ()
  "Add a new line below the current line"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "C-o") 'insert-new-line-below)

;; Move buffers around
(use-package buffer-move
  :ensure t
  :bind (("C-c w <up>"    . buf-move-up)
         ("C-c w <down>"  . buf-move-down)
         ("C-c w <left>"  . buf-move-left)
         ("C-c w <right>" . buf-move-right)))

;; More intuitive regions
;;; This makes the visual region behave more like the contemporary concept of highlighted text, that can be erased or overwritten as a whole.
;;; Source: https://github.com/DiegoVicen/my-emacs
(delete-selection-mode t)

;; Define keybindings to eval-buffer on init
(setq configuration-dir "~/.emacs.d/")

(defun reload-emacs-configuration()
  "Reload the configuration"
  (interactive)
    (load "~/.emacs.d/init.el"))

(defun open-emacs-configuration ()
  "Open the configuration.org file in buffer"
  (interactive)
    (find-file (concat configuration-dir "init.el")))

(global-set-key (kbd "C-c C-b") 'reload-emacs-configuration)
(global-set-key (kbd "C-c C-o") 'open-emacs-configuration)
(global-set-key (kbd "M-j") 'mark-word)

;; Useful keybindings
(unbind-key "C-z")
(bind-key "C-z" 'undo)
(bind-key "C-c f" 'find-file)
(bind-key "C-x e" 'other-frame)
(bind-key "C-c C-v" 'revert-buffer)
(bind-key "C-q" 'kill-buffer)
(bind-key "M-q" 'query-replace-regexp)
(bind-key "s-<" 'beginning-of-buffer)
(bind-key "s->" 'end-of-buffer)
(bind-key "C-c C-w" 'fill-paragraph)


                                        ;---------------------------;
                                        ;--- Graphical Interface ---;
                                        ;---------------------------;

;; Disabling GUI defaults
(if window-system
    (progn
      (toggle-scroll-bar 0)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      ;; Maximize Emacs at startup
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (setq use-default-font-for-symbols nil)
      (setq inhibit-compacting-font-caches t)))

;; Quiet Startup
(defun display-startup-echo-area-message ()
  (message ""))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq frame-title-format nil)

;; Setting default font
(set-face-attribute 'default nil :font "Iosevka")
(setq-default line-spacing 0.001)
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'default nil :weight 'normal)

;; Setting my favorite theme
(use-package darktooth-theme :ensure t  :config  (load-theme 'darktooth t))

;; Set the cursor as a vertical bar
(setq-default cursor-type 'bar)

;; Adding icons with all-the-icons
(use-package all-the-icons
  :ensure t
  :defer t
  :config
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable:
  ;; Source: https://github.com/domtronn/all-the-icons.el
  ;; Source2: https://github.com/domtronn/all-the-icons.el/issues/28
  (setq inhibit-compacting-font-caches t))

                                        ;--------------------------;
                                        ;--- Packages and Tools ---;
                                        ;--------------------------;
;; Ido everywhere
(use-package ido
  :ensure t
  :config
  (setq ido-everywhere t)
  (setq ido-ubiquitous-mode t)
  (setq ido-create-new-buffer 'always)
  (setq ido-file-extensions-order '(".org" ".md" ".markdown" ".c" ".h" ".yaml" ".rb" ".html" ".el" ".ini" ".cfg" ".cnf"))
  (setq ido-enable-flex-matching t)
  (ido-mode 1))

;; Smex
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

;; AG (Silver searcher)
(use-package ag
  :ensure t
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project)
  :config
  (use-package wgrep-ag))

;; Sublime Text moving paragraphs
(use-package move-text
  :ensure t
  :bind
  ("M-<down>" . move-text-down)
  ("M-<up>" . move-text-up))

;; Duplicate things
(use-package duplicate-thing
  :ensure t
  :bind ("C-c C-d" . duplicate-thing))

;; Ace window
(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-select-window))
  
;; Dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  (setq dired-dwim-target t)
    ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  :hook (dired-mode . dired-hide-details-mode))
;; Colourful Dired
(use-package diredfl
  :ensure t
  :init (diredfl-global-mode 1))

;; Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-count-format "%d/%d ")

  :bind (("C-s" . swiper)
         ("M-i" . counsel-imenu)
         :map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         ("C-j" . ivy-done)))

(use-package ivy-rich
  :defer 0.1
  :ensure t
  :preface
  (defun ivy-rich-branch-candidate (candidate)
    "Displays the branch candidate of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize
                 (replace-regexp-in-string abbreviated-home-dir "~/"
                                           (file-name-directory
                                            (directory-file-name candidate)))
                 'face 'font-lock-doc-face)
                (propertize
                 (file-name-nondirectory
                  (directory-file-name candidate))
                 'face 'success)))))

  (defun ivy-rich-compiling (candidate)
    "Displays compiling buffers of the candidate for ivy-rich."
    (let* ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate)
              (not (magit-git-repo-p candidate)))
          ""
        (if (my/projectile-compilation-buffers candidate)
            "compiling"
          ""))))

  (defun ivy-rich-file-group (candidate)
    "Displays the file group of the candidate for ivy-rich"
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
               (group-function (if (fboundp #'group-name) #'group-name #'identity))
               (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))

  (defun ivy-rich-file-modes (candidate)
    "Displays the file mode of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))

  (defun ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))

  (defun ivy-rich-switch-buffer-icon (candidate)
    "Returns an icon for the candidate out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-rich-candidate               (:width 73))
                (ivy-rich-file-user               (:width 8 :face font-lock-doc-face))
                (ivy-rich-file-group              (:width 4 :face font-lock-doc-face))
                (ivy-rich-file-modes              (:width 11 :face font-lock-doc-face))
                (ivy-rich-file-size               (:width 7 :face font-lock-doc-face))
                (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-switch-project
             '(:columns
               ((ivy-rich-branch-candidate        (:width 80))
                (ivy-rich-compiling))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon       (:width 2))
                (ivy-rich-candidate                (:width 40))
                (ivy-rich-switch-buffer-size       (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 20 :face warning)))
               :predicate (lambda (cand) (get-buffer cand))))
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))


;; Counsel
(use-package counsel
  :ensure t
  :config
  ;; Enhance fuzzy matching
  (use-package flx :ensure t)
  ;; Enhance M-x
  (use-package amx :ensure t) ;; https://stackoverflow.com/questions/53026872/m-x-does-not-show-previous-commands
  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :ensure t
    :config (counsel-projectile-mode 1))

  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; iy-go-to-char
;;; Mimic vim’s f with this function.
(use-package iy-go-to-char
  :ensure t
  :demand t
  :bind (("M-m" . iy-go-up-to-char)
         ("M-M" . iy-go-to-char)))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1)
  (define-key projectile-mode-map (kbd "C-C p") 'projectile-command-map))

;; avy-jump
(use-package avy
  :ensure t
  :bind (("M-SPC" . 'avy-goto-char-timer)
         ("M-g a" . 'avy-goto-line)))

;; iedit
;;; This tool allows us to edit all variable names at once just by entering a single keystroke.
(use-package iedit
  :ensure t
  :bind (("C-c i" . iedit-mode)))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :bind(("C-c r" . counsel-recentf))
  :custom
  (recentf-max-saved-items 20)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "recentf"
                     "COMMIT_EDITMSG\\'")))
;; Paren highlighting
(use-package paren
  :ensure t
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Beacon
(use-package beacon
  :ensure t
  :custom
  (beacon-color "#f1fa8c")
  :hook (after-init . beacon-mode))

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

;; Mwin
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; Neotree
(use-package neotree
  :ensure t
  :after
  projectile
  :commands
  (neotree-show neotree-hide neotree-dir neotree-find)
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind
  ("<f8>" . neotree-current-dir-toggle)
  ("<f9>" . neotree-projectile-toggle)
  :preface
  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))
  (defun neotree-current-dir-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
             (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

;; https://emacs.stackexchange.com/questions/37678/neotree-window-not-resizable
(setq neo-window-fixed-size nil)


;; DoReMi
;; https://www.emacswiki.org/emacs/WindowResize
(require 'doremi)
(require 'doremi-cmd)   

(use-package dashboard
  :ensure t
  :defer nil
  :diminish
  (dashboard-mode page-break-lines-mode)
  :preface
  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  (defun my/dashboard-banner ()
    "Set a dashboard banner including information on package initialization
  time and garbage collections."""
    (setq dashboard-banner-logo-title
          (format "E M A C S ready in %.2f seconds with %d garbage collections."
                  (float-time (time-subtract after-init-time before-init-time)) gcs-done))
    (setq dashboard-footer "Gunshow picture #648 by yr friend KC Green")
    (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                       :height 1.1
                                                       :v-adjust -0.05
                                                       :face 'font-lock-keyword-face)))

  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info nil)
  (setq dashboard-startup-banner (concat user-emacs-directory "this-is-fine-gunshow.png"))
  (dashboard-setup-startup-hook)
  ;;
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
        `(;; line1
          ((,(all-the-icons-faicon "home" :height 1.1 :v-adjust 0.0)
            "IOC Website"
            "Open IOC page on your browser"
            (lambda (&rest _) (browse-url "https://ioc.xtec.cat"))
            'default)
           (,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Custom Emacs Commands"
            "List of useful emacs commands"
            (lambda (&rest _) (find-file "~/.emacs.d/myemacs.md"))
            'default)
           (,(all-the-icons-octicon "pencil" :height 1.1 :v-adjust 0.0)
            "Open scratch buffer"
            "Switch to the scratch buffer"
            (lambda (&rest _) (create-scratch-buffer))
            'default)
           (,(all-the-icons-fileicon "emacs" :height 1.1 :v-adjust 0.0)
            "Open init.el"
            "Open Emacs configuration file for easy editing"
            (lambda (&rest _) (find-file "~/.emacs.d/init.el"))
            'default))))
  ;;
  
  :custom
  (dashboard-center-content t)
  (dashboard-items '((recents . 3)
                     (projects . 3)
                     (agenda . 3)))
  :hook ((after-init     . dashboard-refresh-buffer)
         (dashboard-mode . my/dashboard-banner)))




                                        ;_________________________;
                                        ;--- Programming Modes ---;
                                        ;-------------------------;

;; C/C++
(use-package cc-mode
  :ensure t
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

;; Git/Magit
(use-package git-timemachine
  :ensure t
  :bind ("M-g t" . git-timemachine-toggle))

(use-package diffview
  :ensure t
  :commands (diffview-current diffview-region diffview-message))

(use-package magit
  :ensure t
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status))

(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)

(use-package browse-at-remote
  :bind ("M-g r" . browse-at-remote))

(use-package github-pullrequest :ensure t)


;; GNU Global Tags
;; download this pack for windows: http://adoxa.altervista.org/global/
(use-package ggtags
  :ensure t
  :commands ggtags-mode
  :diminish ggtags-mode
  :bind (("M-*" . pop-tag-mark)
         ("C-c t s" . ggtags-find-other-symbol)
         ("C-c t h" . ggtags-view-tag-history)
         ("C-c t r" . ggtags-find-reference)
         ("C-c t f" . ggtags-find-file)
         ("C-c t c" . ggtags-create-tags))
  :init
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
                  (ggtags-mode 1)))))

                                        ;------------------------;
                                        ;--- Web debvelopment ---;
                                        ;------------------------;

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (add-hook 'web-mode-hook 'electric-pair-mode))

(use-package web-beautify
  :ensure t
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))


(defun surround-html (start end tag)
   "Wraps the specified region (or the current 'symbol / word'
 with a properly formatted HTML tag."
   (interactive "r\nsTag: " start end tag)
   (save-excursion
     (narrow-to-region start end)
     (goto-char (point-min))
     (insert (format "<%s>" tag))
     (goto-char (point-max))
     (insert (format "</%s>" tag))
     (widen)))

;; (define-key html-mode-map (kbd "C-c C-w") 'surround-html)

(use-package emmet-mode
  :ensure t
  :diminish (emmet-mode . "ε")
  :bind* (("C-)" . emmet-next-edit-point)
          ("C-(" . emmet-prev-edit-point))
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-strict-trailing-comma-warning t)
  (setq-default js2-strict-inconsistent-return-warning nil)
  :config
  (use-package prettier-js :ensure t)
  (use-package json-mode :ensure t))
  
                                        ;-------------------------;
                                        ;--- Other Major Modes ---;
                                        ;-------------------------;

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
  (markdown-header-face-1 ((t (:foreground "violet" :weight bold))))
  (markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold))))
  (markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold))))
  (markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
  (markdown-list-face ((t (:foreground "mediumpurple"))))
  (markdown-bold-face ((t (:foreground "Yellow" :weight bold))))
  (markdown-pre-face ((t (:foreground "#bd98fe"))))  
  :hook (markdown-mode . visual-line-mode))

(use-package olivetti
  :ensure t
  :config
  (setq-default
   olivetti-hide-mode-line t
   olivetti-body-width 80))

(use-package fountain-mode
  :ensure t
  :config
  (fountain-set-font-lock-decoration 2)
  (set-face-attribute 'fountain-scene-heading nil :foreground "#202226" :weight 'bold)
  (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))
  (add-hook 'fountain-mode-hook (lambda () (turn-on-olivetti-mode)))
  (defun export-to-pdf ()
    (shell-command-to-string (format "afterwriting --config afterwriting-config.json --source %s --pdf --overwrite" buffer-file-name)))
  (add-hook 'after-save-hook #'export-to-pdf))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))

(use-package pdf-tools
  :ensure t
  :demand t
  :config
  (pdf-tools-install t)
  (setq pdf-annot-activate-created-annotations t)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("h" . pdf-annot-add-highlight-markup-annotation)
              ("t" . pdf-annot-add-text-annotation)
              ("D" . pdf-annot-delete)))

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
                                        ;-----------;
                                        ;--- ORG ---;
                                        ;-----------;


(use-package org
  :ensure t
  :ensure org-plus-contrib)

(add-hook 'org-mode-hook 'auto-fill-mode)
(setq-default fill-column 79)

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "DROP(x!)"))
      org-log-into-drawer t)

(setq org-src-tab-acts-natively t)

(defun my/fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)
(setq-default org-image-actual-width 620)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
  (setq org-bullets-bullet-list
        '("◉" "◎" "○" "○" "○" "○")))
(setq org-hide-leading-stars t)

                                        ;---------------;
                                        ;--- ERC/IRC ---;
                                        ;---------------;


;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))
;; auto identify
(setq erc-prompt-for-nickserv-password nil)

(use-package erc
  :custom-face
  (erc-action-face ((t (:foreground "#8fbcbb"))))
  (erc-error-face ((t (:foreground "#bf616a"))))
  (erc-input-face ((t (:foreground "#ebcb8b"))))
  (erc-notice-face ((t (:foreground "#ebcb8b"))))
  (erc-timestamp-face ((t (:foreground "#a3be8c"))))
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-header-line-format "%n on %t (%m)")
  (erc-join-buffer 'bury)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3))
;; Thanks to https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
;; Reddit question: https://www.reddit.com/r/emacs/comments/ekhr95/ircerc_twitch_setup/
;;(setq auth-source-debug t)

;; remember you need to create .authinfo.gpg
;; and then gpg -c .authinfo
;; need to verify if that works with Linux
;; Twitch function
(defun twitch-start-irc ()
  "Connect to Twitch IRC."
  (setq auth-sources '((:source "~/.emacs.d/secrets/.authinfo.gpg")))
  (auth-source-search :host "irc.chat.twitch.tv" :max 1)
  (interactive)
  (when (y-or-n-p "Do you want to start Twitch's IRC? ")
    (erc-tls :server "irc.chat.twitch.tv" :port 6697 :nick "rotterchonsy")))

;; --- end of init.el
