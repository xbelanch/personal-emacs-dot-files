;;; Programming env packages

(use-package magit
  :bind (("C-x g" . magit-status)))

;; source: https://github.com/jwiegley/alert
;; still dunno what it means and how it works...
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'message))

;; The smart-comment project has the nice feature of commenting a line without being at the beginning of the line (default comment in the middle of the line is to split it).
(use-package smart-comment
  :bind ("M-;" . smart-comment))

;;; ---------------------------------------------------------------------------
;;; Company: Completation Anything Anywhere
;;; ---------------------------------------------------------------------------

(use-package company               
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

;;; ---------------------------------------------------------------------------
;;; LOVE GGTAGS!
;;; ---------------------------------------------------------------------------

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


;; With the fancy new ctags-update package, we can update the tags file whenever we save a file:
(use-package ctags-update
  :ensure t
  :config
  (add-hook 'prog-mode-hook  'turn-on-ctags-auto-update-mode)
  :diminish ctags-auto-update-mode)

;;; ---------------------------------------------------------------------------
;;; Restore window configuration after working with gdb window panel
;;; Source: https://emacs.stackexchange.com/questions/26855/how-to-automatically-restore-the-previous-window-layout-after-gdb
;;; ---------------------------------------------------------------------------

;; Select a register number which is unlikely to get used elsewere
(defconst my-windows-config-register 313465989
  "Internal used")

(defvar my-windows-config nil)

(defun set-my-windows-config ()
  (interactive)
  (setq my-windows-config (window-configuration-to-register my-windows-config-register)))

(defun jump-to-my-windows-config ()
  (interactive)
  (jump-to-register my-windows-config-register))

;;; ---------------------------------------------------------------------------
;;; GDB Multiple Windows
;;; ---------------------------------------------------------------------------

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )
