;;;---------------------------------------------------------------------------------------------
;; PDF Tools
;;;---------------------------------------------------------------------------------------------
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
