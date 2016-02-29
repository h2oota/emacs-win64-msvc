(set-language-environment "Japanese")
(setq make-backup-files nil)

(let ((dir "utf8.d")
      (files command-line-args-left))
  (unless (file-exists-p dir)
    (make-directory dir))
  (while files
    (let* ((file (car files))
           (out (expand-file-name
                 (file-name-nondirectory file) dir)))
      (message "%s => %s" file out)
      (with-temp-buffer
        (set-buffer-file-coding-system 'utf-8-unix)
        (insert-file-contents file)
        (write-file out))
      (setq files (cdr files)))))
