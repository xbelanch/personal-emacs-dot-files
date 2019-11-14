                                        ; === Free Belanche Foundation (cc) 2019   ===
                                        ; === Load packages ===

;; Used packages:
;; delsel autorevert hungry-delete smartparens recentf undo-tree projectile wgrep ag anzu mwim move-text duplicate-thing all-the-icons posframe dashboard projectile
;; Load packages
;; If package-check-signature is allow-unsigned, don't
;; signal error when we can't verify signature because of
;; missing public key.  Other errors are still treated as
;; fatal (bug#17625).
;; font: https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
(setq package-check-signature nil)

(eval-when-compile
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'bind-key))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)

  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))



                                        ; === Default init variables ===

(setq line-width-characters 80)

                                        ; ===  Default Encoding ===

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
;; Exemple of yank url: 瑨灴㩳⼯瑳捡潫敶晲潬⹷潣⽭畱獥楴湯⽳㤹㔵㈷⼵獵湩ⵧ汣灩潢牡ⵤ潣祰瀭獡整爭獥汵獴椭⵮档湩獥ⵥ潬歯湩ⵧ档牡捡整獲搭扥慩⵮楳d
;; Now it works: https://stackoverflow.com/questions/9955725/using-clipboard-copy-paste-results-in-chinese-looking-characters-debian-sid
                                        ; (set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8) ; nil
(setq process-coding-system-alist
      (cons '("grep" utf-8 . utf-8) process-coding-system-alist))


                                        ;===  Quiet Startup ===
(defun display-startup-echo-area-message ()
  (message ""))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
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


                                        ; === Basic editing conf packages ===
;; How I missing you some much!
;; Increment/decrement integer at point
(use-package evil-numbers
  :bind (("<kp-add>" . evil-numbers/inc-at-pt)
         ("<kp-subtract>" . evil-numbers/dec-at-pt)))

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
  (sp-pair "[" "]" :actions '(wrap))
  (sp-pair "(" ")" :actions '(wrap))
  (sp-pair "*" "*" :actions '(wrap))
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


                                        ; === gui basic configuration ===

(if window-system
    (progn
      ;; UI parts
      (toggle-scroll-bar 0)
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      (add-to-list 'default-frame-alist '(fullscreen . maximized))

      (setq use-default-font-for-symbols nil)
      (setq inhibit-compacting-font-caches t)))

                                        ; === Set font and size ==== ;
;; Pragmata Pro is for rich people
;; Try the slender version ;)
(set-face-attribute 'default nil :family "Iosevka Nerd Font")
(set-face-attribute 'default nil :height 120)


(use-package all-the-icons
  :defer t)
(use-package posframe)



                                        ; === keybindings ===

;; misc useful keybindings
(bind-key "C-c C-b" 'eval-buffer)
(bind-key "C-z" 'undo)
(bind-key "C-c f" 'find-file)
(bind-key "C-x e" 'other-frame)
(bind-key "C-c C-v" 'revert-buffer)
(bind-key "C-q" 'kill-buffer)
(bind-key "M-q" 'query-replace-regexp)
(bind-key "s-<" 'beginning-of-buffer)
(bind-key "s->" 'end-of-buffer)
(bind-key "C-c C-w" 'fill-paragraph)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))


                                        ; === Hydra  === 

(use-package hydra)

                                        ; === undo tree ===

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

                                        ; === search replace ===

(use-package projectile
  :diminish
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


                                        ; === Ivy ===

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)))


(use-package ivy-rich
  :defer 0.1
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
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))



                                        ; === Swiper ===

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-%" . swiper-query-replace)))

                                        ; === counsel ===

(use-package counsel
  :ensure t
  :config
  ;; Enhance fuzzy matching
  (use-package flx)
  ;; Enhance M-x
  (use-package amx) ;; https://stackoverflow.com/questions/53026872/m-x-does-not-show-previous-commands
  ;; Ivy integration for Projectile
  (use-package counsel-projectile
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

                                        ; === anzu === 

;; https://github.com/syohex/emacs-anzu
(use-package anzu
  :diminish
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode))

                                        ; === ace-window ===

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-c C-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

                                        ; === mwim === 

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))


                                        ; === company ===

(use-package company
  :diminish company-mode
  :ensure t
  :config
  ;; using child frame
  (use-package company-posframe
    :hook (company-mode . company-posframe-mode))
  (setq company-idle-delay 0.5)
  ;; (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

                                        ; === GIT ===

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status))

(use-package gitattributes-mode :defer t)
(use-package gitconfig-mode :defer t)
(use-package gitignore-mode :defer t)

(use-package browse-at-remote
  :bind ("M-g r" . browse-at-remote))

(use-package github-pullrequest)


                                        ; === TOOLS ===

(use-package google-translate
  :bind
  ("M-o t" . google-translate-at-point)
  ("M-o T" . google-translate-at-point-reverse)
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ca"))


(use-package google-this)


                                        ; === LANGUAGES ===

                                        ;=== C/C++ ===
(defun compile-immediate ()
  (interactive)
  (custom-set-variables
   '(compilation-read-command nil))
  (call-interactively 'compile))

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

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

  (mapc (lambda (map)
          (bind-key "C-c c" 'compile-immediate map)
          (bind-key "C-c n" 'next-error map)
          (bind-key "C-c p" 'previous-error map))
        (list c-mode-map
              c++-mode-map)))


                                        ; === Javascript ===

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode))


                                        ; === Web mode ===

;; web-mode: An autonomous emacs major-mode for editing web templates.
;; http://web-mode.org/
(use-package web-mode
  :defer t
  :init
  (setq-default
   web-mode-code-indent-offset 4
   web-mode-comment-style 4
   web-mode-css-indent-offset 4
   web-mode-enable-current-element-highlight t
   web-mode-enable-current-column-highlight t
   web-mode-markup-indent-offset 4)
  :mode
  ("\\.erb\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.tpl\\'" . web-mode))

                                        ; === Markdown ===

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
  (markdown-header-face-1 ((t (:foreground "violet" :weight bold :height 1.9))))
  (markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold :height 1.6))))
  (markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold :height 1.4))))
  (markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
  (markdown-list-face ((t (:foreground "mediumpurple"))))
  (markdown-bold-face ((t (:foreground "Yellow" :weight bold))))
  (markdown-pre-face ((t (:foreground "#bd98fe"))))  
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook #'smartparens-mode))

(use-package markdown-mode+
  :after markdown-mode)

(use-package markdown-toc
  :ensure t
  :config
  (setq markdown-toc-header-toc-title "# Índex"))


                                        ; === Pandoc copyit ===

(use-package copyit :ensure t)
(use-package copyit-pandoc :ensure t)

                                        ; === Olivetti ===

(use-package olivetti
  :config
  (setq-default
   olivetti-hide-mode-line t
   olivetti-body-width line-width-characters))

                                        ; === Fountain ===

(use-package fountain-mode
  :config

  (fountain-set-font-lock-decoration 2)
  (set-face-attribute 'fountain-scene-heading nil :foreground "#202226" :weight 'bold)

  (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))
  (add-hook 'fountain-mode-hook (lambda () (turn-on-olivetti-mode)))
  (defun export-to-pdf ()
    (shell-command-to-string (format "afterwriting --config afterwriting-config.json --source %s --pdf --overwrite" buffer-file-name)))
  (add-hook 'after-save-hook #'export-to-pdf))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))


                                        ; === YAML ===

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))


                                        ; === CUSTOM FUNCTIONS ===

;;; (#2): Broken paragraphs (markdown files) 
;;; unfill.el --- Unfill paragraphs or regions, and toggle between filled & unfilled
;;; source: https://github.com/purcell/unfill/blob/master/unfill.el

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

(defun unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun unfill-toggle ()
  "Toggle filling/unfilling of the current region, or current paragraph if no region active."
  (interactive)
  (let (deactivate-mark
        (fill-column
         (if (eq last-command this-command)
             (progn (setq this-command nil)
                    most-positive-fixnum)
           fill-column)))
    (call-interactively 'fill-paragraph)))

(define-obsolete-function-alias 'toggle-fill-unfill 'unfill-toggle)


;; source: https://www.emacswiki.org/emacs/TransposeWindows
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


(defun put-current-path-to-clipboard ()
    (interactive)
    (let ((file-path buffer-file-name)
          (dir-path default-directory))
      (cond (file-path
             (kill-new (expand-file-name file-path))
             (message "This file path is on the clipboard!"))
            (dir-path
             (kill-new (expand-file-name dir-path))
             (message "This directory path is on the clipboard!"))
            (t
             (error-message-string "Fail to get path name.")))))

  (defun put-current-filename-to-clipboard ()
    (interactive)
    (let ((file-path buffer-file-name)
          (dir-path default-directory))
      (cond (file-path
             (kill-new (file-name-nondirectory file-path))
             (message "This file path is on the clipboard!"))
            (dir-path
             (kill-new (file-name-nondirectory dir-path))
             (message "This directory path is on the clipboard!"))
            (t
             (error-message-string "Fail to get path name.")))))

  (defun put-current-filename-with-line-to-clipboard ()
    (interactive)
    (let ((file-path buffer-file-name)
          (dir-path default-directory))
      (cond (file-path
             (kill-new (format "%s:%s"
                               (file-name-nondirectory file-path)
                               (count-lines (point-min) (point))))
             (message "This file path is on the clipboard!"))
            (dir-path
             (kill-new (file-name-nondirectory dir-path))
             (message "This directory path is on the clipboard!"))
            (t
             (error-message-string "Fail to get path name.")))))

                                        ; === UI ===

                                        ; === Dashboard ===

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
  (dashboard-center-content t)
  (dashboard-items '((recents . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  :hook ((after-init     . dashboard-refresh-buffer)
         (dashboard-mode . my/dashboard-banner)))

                                        ; === i-menu-list ===
(use-package imenu-list
  :bind
  ("<f10>" . imenu-list-smart-toggle)
  :custom-face
  (imenu-list-entry-face-1 ((t (:foreground "white"))))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

                                        ; === neotree ===

(use-package neotree
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


                                        ; === darktooth-theme

(use-package darktooth-theme
  :ensure t
  :config
  (load-theme 'darktooth t))

                                        ; === nyan-mode ===

(use-package nyan-mode
  :if window-system
  :ensure t
  :config
  (nyan-mode)
  (nyan-start-animation))


                                        ; === dimmer ===
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


                                        ; === HIGHLIGTHS ===

(use-package hl-line
  :ensure nil
  :hook
  (after-init . global-hl-line-mode))


(use-package paren
  :ensure nil
  :hook
  (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c")))) ;; :box t
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package highlight-symbol
  :bind
  (:map prog-mode-map
  ("M-o h" . highlight-symbol)
  ("M-p" . highlight-symbol-prev)
  ("M-n" . highlight-symbol-next)))

(use-package beacon
  :custom
  (beacon-color "#f1fa8c")
  :hook (after-init . beacon-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package volatile-highlights
  :diminish
  :hook
  (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

(use-package highlight-indent-guides
  :diminish
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)) ; column

                                        ; === PDF TOOLS  ===

;; http://www.sigmafield.org/2009/10/03/using-doc-view-with-auto-revert-to-view-latex-pdf-output-in-emacs
(add-hook 'doc-view-mode-hook #'auto-revert-mode)

(use-package pdf-tools
  ;; https://github.com/zakame/.emacs.d/blob/379dbfe0f10b20f7f43054cd4d13303d8026d105/init.el#L596-L603
  :if (and (string= system-type 'gnu/linux)
           (eq (call-process-shell-command "pkg-config" nil nil nil "--exists" "poppler") 0))
  :commands (pdf-tools-install
             modi/pdf-tools-re-install)
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn
    (defvar modi/pdf-tools-bin-directory (let* ((dir-1 (file-name-as-directory (expand-file-name "misc" user-emacs-directory)))
                                                (dir-2 (file-name-as-directory (expand-file-name "pdf-tools" dir-1)))
                                                (dir (file-name-as-directory (expand-file-name "bin" dir-2))))
                                           (make-directory dir :parents)
                                           dir)
      "Directory to hold the executable(s) for pdf-tools.")

    (setq-default pdf-view-display-size 'fit-page) ; fit page by default
    (setq pdf-view-resize-factor 1.10)

    (setq pdf-info-epdfinfo-program (expand-file-name "epdfinfo" modi/pdf-tools-bin-directory))

    ;; https://github.com/politza/pdf-tools/issues/312#issuecomment-329537742
    ;; Build the program (if necessary) without asking first, if NO-QUERY-P is
    ;; non-nil.
    (pdf-tools-install :no-query-p)

    (defun modi/pdf-tools-re-install ()
      "Re-install `epdfinfo' even if it is installed.
The re-installation is forced by deleting the existing `epdfinfo'
binary.
Useful to run after `pdf-tools' updates."
      (interactive)
      (when (pdf-info-running-p)
        (pdf-info-kill))
      (delete-file pdf-info-epdfinfo-program)
      (pdf-tools-install :no-query-p))

    ;; Update `pdf-view-mode-map' bindings
    (dolist (pair '((beginning-of-buffer . pdf-view-first-page)
                    (end-of-buffer . pdf-view-last-page)
                    (modi/scroll-up . pdf-view-next-line-or-next-page)
                    (modi/scroll-down . pdf-view-previous-line-or-previous-page)))
      (let ((remap-from (car pair))
            (remap-to (cdr pair)))
        (define-key pdf-view-mode-map `[remap ,remap-from] remap-to)))

    (bind-keys
     :map pdf-view-mode-map
     ("l" . pdf-history-backward)
     ("r" . pdf-history-forward))))


                                        ; === C Compiling, debugging, tags ====

                                        ; === gdb ===

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )


                                        ; === ggtags ====
;; GNU Global Tags
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


                                        ; === MISC ===

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


                                        ;=== END OF init.el ===
