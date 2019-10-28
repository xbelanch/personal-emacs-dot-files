;; load path and set packages sources

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
    (package-install 'quelpa)
    (package-install 'bind-key))

  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)

  (require 'use-package))


(require 'diminish)
(require 'bind-key)

;;;----------------------------------------------------------------------------
;;; Setting edition options and related stuff
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/editing.el")
  (load "~/.emacs.d/editing.el"))

;;;----------------------------------------------------------------------------
;;; Setting ui options and related stuff
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/ui.el")
  (load "~/.emacs.d/ui.el"))

;;;----------------------------------------------------------------------------
;;; Setting key bindings options and related stuff
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/keybindings.el")
  (load "~/.emacs.d/keybindings.el"))
        
;;;----------------------------------------------------------------------------
;;; Setting completion functions
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/completion.el")
  (load "~/.emacs.d/completion.el"))

;;;----------------------------------------------------------------------------
;;; Settings git options
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/git.el")
  (load "~/.emacs.d/git.el"))

;;;----------------------------------------------------------------------------
;;; Settings google-tools functions
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/google-tools.el")
  (load "~/.emacs.d/google-tools.el"))

;;;----------------------------------------------------------------------------
;;; Settings programming and markup languages options
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/languages.el")
  (load "~/.emacs.d/languages.el"))

;;;----------------------------------------------------------------------------
;;; Settings custom functions
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/custom-functions.el")
  (load "~/.emacs.d/custom-functions.el"))

;;;----------------------------------------------------------------------------
;;; Settings hightline functions
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/hightline.el")
  (load "~/.emacs.d/hightline.el"))

;;;----------------------------------------------------------------------------
;;; Setting counsel-ivy options and related stuff
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/ivy.el")
  (load "~/.emacs.d/ivy.el"))

;;;----------------------------------------------------------------------------
;;; Setting up weather and forescast info
;;;---------------------------------------------------------------------------
;; (when (file-exists-p "~/.emacs.d/weather.el")
;;   (load "~/.emacs.d/weather.el"))

;;;----------------------------------------------------------------------------
;;; Setting up pdf-tools
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/pdf-tools.el")
  (load "~/.emacs.d/pdf-tools.el"))

;;;----------------------------------------------------------------------------
;;; Setting up treemacs
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/treemacs.el")
  (load "~/.emacs.d/treemacs.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highligh-search t t)
 '(ag-reuse-buffers t t)
 '(ag-reuse-window t t)
 '(all-the-icons-ivy-buffer-commands (quote (ivy-switch-buffer-other-window)))
 '(beacon-color "#f1fa8c")
 '(company-echo-delay 0 t)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.8)
 '(counsel-rg-base-command "rg -S -M 150 --no-heading --line-number --color never %s")
 '(dashboard-banner-logo-title "Welcome to Free Belanche Foundation" t)
 '(dashboard-center-content t)
 '(dashboard-footer "Emacs is pretty cool!" t)
 '(dashboard-footer-icon
   #("" 0 1
     (face
      (:family "github-octicons" :height 1.32 :inherit font-lock-keyword-face)
      font-lock-face
      (:family "github-octicons" :height 1.32 :inherit font-lock-keyword-face)
      display
      (raise -0.06)
      rear-nonsticky t)) t)
 '(dashboard-items (quote ((recents . 15) (projects . 5) (bookmarks . 5))) t)
 '(dashboard-startup-banner "./mriocbot.png" t)
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project))
 '(doom-modeline-icon t)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-minor-modes nil)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(enable-recursive-minibuffers t)
 '(font-lock-maximum-decoration (quote ((fountain-mode) (t . t))))
 '(google-translate-default-source-language "en" t)
 '(google-translate-default-target-language "ca" t)
 '(highlight-indent-guides-auto-enabled t)
 '(highlight-indent-guides-method (quote character))
 '(highlight-indent-guides-responsive t)
 '(imenu-list-auto-resize t t)
 '(imenu-list-focus-after-activation t t)
 '(ivy-case-fold-search-default t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-re-builders-alist (quote ((t . ivy--regex-plus))) t)
 '(ivy-use-virtual-buffers t)
 '(magit-auto-revert-mode nil)
 '(markdown-bold-underscore t)
 '(markdown-enable-math t)
 '(markdown-header-scaling t)
 '(markdown-hide-markup nil)
 '(markdown-hide-urls nil)
 '(markdown-indent-function t)
 '(markdown-italic-underscore t)
 '(neo-theme (quote icons) t)
 '(nyan-animate-nyancat t)
 '(nyan-cat-face-number 4)
 '(package-selected-packages
   (quote
    (treemacs-magit treemacs-icons-dired treemacs-projectile treemacs sotlisp solar sunshine all-the-icons-ivy yaml-mode systemd logview markdown-toc markdown-mode js2-mode google-this google-translate github-pullrequest browse-at-remote gitignore-mode gitconfig-mode gitattributes-mode magit diffview git-timemachine company-quickhelp company-box company-posframe company mwim ivy-rich ivy-posframe counsel-projectile amx flx counsel wgrep-ag anzu ag wgrep projectile undo-tree hydra-posframe hydra which-key point-history popwin posframe all-the-icons smartparens hungry-delete bind-key quelpa diminish use-package)))
 '(recentf-auto-cleanup (quote never) t)
 '(recentf-exclude
   (quote
    ((expand-file-name package-user-dir)
     ".cache" "cache" "recentf" "COMMIT_EDITMSG\\'")) t)
 '(recentf-max-saved-items 20000000 t)
 '(show-paren-style (quote mixed))
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(swiper-action-recenter t)
 '(wgrep-auto-save-buffer t t)
 '(wgrep-change-readonly-file t t)
 '(wgrep-enable-key "e" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "violet"))))
 '(hydra-posframe-border-face ((t (:background "#6272a4"))))
 '(imenu-list-entry-face-1 ((t (:foreground "white"))))
 '(ivy-posframe ((t (:background "#282a36"))))
 '(ivy-posframe-border ((t (:background "#6272a4"))))
 '(ivy-posframe-cursor ((t (:background "#61bfff"))))
 '(markdown-bold-face ((t (:foreground "Yellow" :weight bold))))
 '(markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :foreground "LightSkyBlue1" :weight bold :family "Helvetica"))))
 '(markdown-header-face-1 ((t (:foreground "violet" :weight bold :height 1.9))))
 '(markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold :height 1.6))))
 '(markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold :height 1.4))))
 '(markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
 '(markdown-list-face ((t (:foreground "mediumpurple"))))
 '(markdown-pre-face ((t (:foreground "#bd98fe"))))
 '(show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
 '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))
