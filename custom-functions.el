(defun upcase-backward-word (arg)
  (interactive "p")
  (upcase-word (- arg)))

(defun downcase-backward-word (arg)
  (interactive "p")
  (downcase-word (- arg)))

(defun capitalize-backward-word (arg)
  (interactive "p")
  (capitalize-word (- arg)))

(global-set-key (kbd "C-M-u")	 'upcase-backward-word)
(global-set-key (kbd "C-M-l")	 'downcase-backward-word)
(global-set-key (kbd "M-c")	 'capitalize-backward-word)

(defun kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~],.??" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

(global-set-key (kbd "M-d")  'kill-word-at-point)

(defun backward-kill-word-or-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

(global-set-key (kbd "C-w")  'backward-kill-word-or-region)

(defun add-chmod()
    (interactive)
    (save-restriction
      (widen)
      (let ((name (buffer-file-name)))
        (if (and (not (string-match ":" name))
                 (not (string-match "/\\.[^/]+$" name))
                 (equal "#!" (buffer-substring 1 (min 3 (point-max)))))
            (progn (set-file-modes name (logior (file-modes name) 73))
                   (message "Wrote %s (chmod +x)" name))))))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

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
