;; Writing settings for Markdown, Fountain...

;;; Need to write a simple pandoc call or better to pandocomatic instead
;;; pandoc-mode :-(
;;; Time to learn a bit of elisp?

;;; Super markdown-mode
(use-package markdown-mode
    :init
    :config
    (setq markdown-xhtml-header-content "<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />")
    ;; (add-hook 'markdown-mode-hook (lambda () (turn-on-olivetti-mode)))
    :mode "\\.md\\'"
    :ensure t)

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :foreground "LightSkyBlue1" :weight bold :family "Helvetica"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.9))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-bold-face ((t (:inherit font-lock-function-name-face :foreground "Yellow" :weight bold))))
 ;; '(markdown-italic-face ((t (:inherit font-lock-function-name-face :foreground "LightSkyBlue3"))))
 )

(add-hook 'markdown-mode-hook
          (lambda ()
             (setq whitespace-mode -1)))

(use-package markdown-toc
  :ensure t
  :config
  (setq markdown-toc-header-toc-title "# Índex"))

(use-package olivetti
  :config
  (setq-default
   olivetti-hide-mode-line t
   olivetti-body-width line-width-characters))

(use-package fountain-mode
  :config

  (fountain-set-font-lock-decoration 2)
  (set-face-attribute 'fountain-scene-heading nil :foreground "#202226" :weight 'bold)

  (add-to-list 'auto-mode-alist '("\\.fountain$" . fountain-mode))
  (add-hook 'fountain-mode-hook (lambda () (turn-on-olivetti-mode)))
  (defun export-to-pdf ()
    (shell-command-to-string (format "afterwriting --config afterwriting-config.json --source %s --pdf --overwrite" buffer-file-name)))
  (add-hook 'after-save-hook #'export-to-pdf))


