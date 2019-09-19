;; 2019 Free Belanche Foundation
;; Default Emacs Config Files

;;--------------------------------------------------------------------------
;;; Package sources font
;;--------------------------------------------------------------------------
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))

;;----------------------------------------------------------------------------
;; Use package ;) THAT'S SO IMPORTANT!
;;----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'diminish)

;;----------------------------------------------------------------------------
;; bind-key.el --- A simple way to manage personal keybindings
;; Source: https://github.com/jwiegley/use-package/blob/master/bind-key.el
;;----------------------------------------------------------------------------
(use-package bind-key)

;; misc useful keybindings
(bind-key "C-c C-b" 'eval-buffer)
(bind-key "C-z" 'undo)
(bind-key "C-c f" 'find-file)
(bind-key "C-x e" 'other-frame)
(bind-key "C-c C-v" 'revert-buffer)
(bind-key "C-c <left>" 'windmove-left)
(bind-key "C-c <right>" 'windmove-right)
(bind-key "C-c <up>" 'windmove-up)
(bind-key "C-c <down>" 'windmove-down)
(bind-key "C-q" 'kill-buffer)
(bind-key "M-q" 'query-replace-regexp)
(bind-key "s-<" 'beginning-of-buffer)
(bind-key "s->" 'end-of-buffer)
(bind-key "C-c C-w" 'fill-paragraph)
(fset 'yes-or-no-p 'y-or-n-p)

;;;----------------------------------------------------------------------------
;;; Setting GUI Options and related stuff
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/gui.el")
  (load "~/.emacs.d/gui.el"))


;;;----------------------------------------------------------------------------
;;; Setting Editing options
;;;---------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/editing.el")
  (load "~/.emacs.d/editing.el"))

;;; --------------------------------------------------------------------------------
;;; Setting up programming mode (C, Ruby, Javascript...)
;;; --------------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/programming.el")
  (load "~/.emacs.d/programming.el"))

;;; --------------------------------------------------------------------------------
;;; Setting up writing mode (Markdown, Fountain...)
;;; --------------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/writing.el")
  (load "~/.emacs.d/writing.el"))

;;; --------------------------------------------------------------------------------
;;; Setting up miscellaneous modes (Weather and other stuff)
;;; --------------------------------------------------------------------------------
(when (file-exists-p "~/.emacs.d/misc.el")
  (load "~/.emacs.d/misc.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-maximum-decoration (quote ((fountain-mode) (t . t))))
 '(package-selected-packages
   (quote
    (org org-beautify-theme org-bullets pandoc-mode copyit-pandoc yaml-mode duplicate-thing move-text fountain-mode olivetti pdf-tools forecast sunshine beacon smart-mode-line smart-mode-line-powerline-theme treemacs ace-window nyan-mode ggtags ctags-update smart-comment avy smex golden-ratio diminish markdown-mode markdown-toc company alert magit undo-tree counsel swiper dracula-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-bold-face ((t (:inherit font-lock-function-name-face :foreground "Yellow" :weight bold))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :foreground "LightSkyBlue1" :weight bold :family "Helvetica"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.9))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4)))))
