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
