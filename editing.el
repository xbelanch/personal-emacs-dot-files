;;; redistribute windows related to golden ratio
;; (use-package golden-ratio
;;   :config
;;   (golden-ratio-mode))
	

;;; Warnings, Alerts and other special keywords in comments
;; from Casey Muratori
(setq fixme-modes '(c-mode fountain-mode markdown-mode))
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
(modify-face 'font-lock-fixme-face "Magenta" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "cyan" nil nil t nil t nil nil)
(modify-face 'font-lock-done-face "Green" nil nil t nil t nil nil)
(modify-face 'font-lock-alert-face "OrangeRed" nil nil t nil t nil nil)
(modify-face 'font-lock-hack-face "gold" nil nil t nil t nil nil)


;;; nice cat to show up how long the document is!
(use-package nyan-mode
  :if window-system
  :ensure t
  :config
  (nyan-mode)
  (nyan-start-animation)
)

;; ace-window
(use-package ace-window
  :ensure t
  :defer t
  :bind (("M-p" . ace-window)))

(use-package smex)
(use-package avy
  :bind(("C-," . avy-goto-char-2)
	("C-;" . avy-goto-word-1)
	("C-'" . avy-goto-line)))

(use-package ivy
  :init
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode))

(use-package recentf-mode
  :bind(("C-c r" . counsel-recentf)))

(use-package swiper				   
  :preface					   
  (defun swiper-at-point ()			   
    (interactive)				   
    (swiper (thing-at-point 'word)))		   
  :bind (("M-s s" . swiper)			   
	 ("M-s M-s" . swiper-at-point)))	   
						   
(use-package counsel				   
  :config					   
  (counsel-mode))				   


(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode 1)
  :config
  (defalias 'redo 'undo-tree-redo)
  :bind (("C-z" . undo)     ; Zap to character isn't helpful
         ("C-S-z" . redo)))

;; Never lose your cursor again
;; source: https://github.com/Malabarba/beacon
(use-package beacon
  :config
  (progn
    (setq beacon-blink-when-point-moves-vertically nil) ; default nil
    (setq beacon-blink-when-point-moves-horizontally nil) ; default nil
    (setq beacon-blink-when-buffer-changes t) ; default t
    (setq beacon-blink-when-window-scrolls t) ; default t
    (setq beacon-blink-when-window-changes t) ; default t
    (setq beacon-blink-when-focused nil) ; default nil

    (setq beacon-blink-duration 0.3) ; default 0.3
    (setq beacon-blink-delay 0.3) ; default 0.3
    (setq beacon-size 30) ; default 40
    (setq beacon-color "yellow") ; default 0.5
    (setq beacon-color 0.5) ; default 0.5

    (add-to-list 'beacon-dont-blink-major-modes 'term-mode)

    (beacon-mode 1)))

;;; Move paragraphs or text like Sublime Text
(unless (package-installed-p 'move-text)
  (package-refresh-contents)
  (package-install 'move-text))

(require 'move-text)
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "M-<up>") 'move-text-up)

(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)


(show-paren-mode)
