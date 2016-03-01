;; -*- mode: Emacs-Lisp; coding: utf-8-unix -*-
;;
;; cmigemo.el - Japanese incremental search trough dynamic pattern expansion
;; Derived from migemo.el v 1.8 by Satoru Takabayashi.

;; Author: HIROSHI OOTA
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

(require 'character-fold)
(require 'mod-cmigemo)
(isearch-define-mode-toggle cmigemo "m" cmigemo-search-pattern-get "\
Turning on cmigemo search turns off regexp mode.")

;;; Code:
(defgroup cmigemo nil
  "cmigemo - Japanese incremental search trough dynamic pattern expansion."
  :group 'matching)

(defcustom cmigemo-detect-default-mode t
  "if t, detect buffer's coding system and choose migemo or search-default-mode, "
  :group 'cmigemo
  :type 'boolean)

(defcustom cmigemo-white-space-regexp "[ 　\t\r\n]*"
  "*Regexp representing white spaces."
  :group 'cmigemo
  :type 'string)

(defcustom cmigemo-coding-system 'utf-8-unix
  "*Default coding system for cmigemo.el"
  :group 'cmigemo
  :type 'coding-system)

(defcustom cmigemo-directory
  (expand-file-name "utf8.d" data-directory)
  "*Directory where cmigemo files are placed"
  :group 'cmigemo
  :type 'directory)

(defcustom cmigemo-dictionary
  (expand-file-name "migemo-dict" cmigemo-directory)
  "*Migemo dictionary file."
  :group 'cmigemo
  :type '(file :must-match t))

(defcustom cmigemo-user-dictionary nil
  "*cmigemo user dictionary file."
  :group 'cmigemo
  :type '(choice (file :must-match t)
		 (const :tag "Do not use" nil)))

(defcustom cmigemo-regex-dictionary nil
  "*cmigemo regex dictionary file."
  :group 'cmigemo
  :type '(choice (file :must-match t)
		 (const :tag "Do not use" nil)))

(defcustom cmigemo-pre-conv-function nil
  "*Function of cmigemo pre-conversion."
  :group 'cmigemo
  :type '(choice (const :tag "Do not use" nil)
		 function))

(defcustom cmigemo-after-conv-function nil
  "*Function of cmigemo after-conversion."
  :group 'cmigemo
  :type '(choice (const :tag "Do not use" nil)
		 function))

(defcustom cmigemo-use-pattern-alist nil
  "*Use pattern cache."
  :group 'cmigemo
  :type 'boolean)

(defcustom cmigemo-use-frequent-pattern-alist nil
  "*Use frequent patttern cache."
  :group 'cmigemo
  :type 'boolean)

(defcustom cmigemo-pattern-alist-length 512
  "*Maximal length of cmigemo-pattern-alist."
  :group 'cmigemo
  :type 'integer)

(defcustom cmigemo-pattern-alist-file "~/.cmigemo-pattern"
  "*Path of cmigemo alist file. If nil, don't save and restore the file."
  :group 'cmigemo
  :type 'file)

(defcustom cmigemo-frequent-pattern-alist-file "~/.cmigemo-frequent"
  "*Path of cmigemo frequent alist file. If nil, don't save and restore the file."
  :group 'cmigemo
  :type 'file)

(defcustom cmigemo-isearch-min-length 1
  "*Minimum length of word to start isearch."
  :group 'cmigemo
  :type 'integer)

;; internal variables
(defvar cmigemo-buffer nil)
(defvar cmigemo-current-input-method nil)
(defvar cmigemo-search-pattern nil)
(defvar cmigemo-pattern-alist nil)
(defvar cmigemo-frequent-pattern-alist nil)
(defvar cmigemo-search-pattern-alist nil)
(defvar cmigemo-do-isearch nil)
(defvar *mod-cmigemo-handle* nil)

(defconst coding-system-for-japanese
  (let ((cs (cdr (assoc 'coding-system
			(assoc  "Japanese" language-info-alist)))))
    (append
     '(utf-8 utf-8-unix utf-8-dos utf-8-mac
             utf-8-auto-unix utf-8-auto-dos utf-8-auto-mac)
     (mapcar
      (lambda (c)
	  (intern-soft (concat (symbol-name c) "-unix"))) cs)
     (mapcar
      (lambda (c)
	  (intern-soft (concat (symbol-name c) "-dos"))) cs)
     (mapcar
      (lambda (c)
	  (intern-soft (concat (symbol-name c) "-mac"))) cs))))

(defun cmigemo-search-pattern-get (string &optional lax)
  (or (cdr (assoc string cmigemo-search-pattern-alist))
      (let ((pattern (cmigemo-get-pattern string)))
        (setq cmigemo-search-pattern-alist
              (cons (cons string pattern)
                    cmigemo-search-pattern-alist))
        pattern)))

(defun cmigemo-replace-in-string (string from to)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((migemo-do-isearch nil))
      (while (search-forward from nil t)
        (replace-match to nil t)))
    (buffer-substring (point-min) (point-max))))

(defun cmigemo-get-pattern (word)
  (if (< (length word) cmigemo-isearch-min-length)
      ""

;      (set-text-properties 0 (length word) nil word)
      (cmigemo-init)
      (when (and cmigemo-pre-conv-function
		 (functionp cmigemo-pre-conv-function))
	(setq word (funcall cmigemo-pre-conv-function word)))
      (let ((pattern
             (let (temp)
               (cond
                ((setq temp (and cmigemo-use-frequent-pattern-alist
                                 (assoc word cmigemo-frequent-pattern-alist)))
                 (cdr temp))
                ((setq temp (and cmigemo-use-pattern-alist
                                 (assoc word cmigemo-pattern-alist)))
                 (setq cmigemo-pattern-alist
                       (cons temp (delq temp cmigemo-pattern-alist)))
                 (cdr temp))
                (t
                 (let ((pattern (cmigemo-query *mod-cmigemo-handle* word)))
                   (when cmigemo-use-pattern-alist
                     (setq cmigemo-pattern-alist
                           (cons (cons word pattern) cmigemo-pattern-alist))
                     (when (and cmigemo-pattern-alist-length
                                (> (length cmigemo-pattern-alist)
                                   (* cmigemo-pattern-alist-length 2)))
                       (setcdr (nthcdr (1- (* cmigemo-pattern-alist-length 2))
                                       cmigemo-pattern-alist) nil)))
                   pattern))))))
        (if (and cmigemo-after-conv-function
                 (functionp cmigemo-after-conv-function))
            (funcall cmigemo-after-conv-function word pattern)
          (cmigemo-replace-in-string pattern "\a"
                                     cmigemo-white-space-regexp)))))

(defun cmigemo-init ()
  (when (and cmigemo-use-frequent-pattern-alist
	     cmigemo-frequent-pattern-alist-file
	     (null cmigemo-frequent-pattern-alist))
    (setq cmigemo-frequent-pattern-alist
	  (cmigemo-pattern-alist-load cmigemo-frequent-pattern-alist-file)))

  (when (and cmigemo-use-pattern-alist
	     cmigemo-pattern-alist-file
	     (null cmigemo-pattern-alist))
    (setq cmigemo-pattern-alist
	  (cmigemo-pattern-alist-load cmigemo-pattern-alist-file)))

  (setq *mod-cmigemo-handle*
        (cmigemo-open cmigemo-dictionary)))

(defun cmigemo-pattern-alist-load (file)
  "Load cmigemo alist file."
  (let ((coding-system-for-read cmigemo-coding-system))
    (setq file (expand-file-name file))
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(condition-case err
	    (read (current-buffer))
	  (error
	   (message "Error while reading %s; %s"
		    (file-name-nondirectory file)
		    (error-message-string err))
	   nil))))))

(defun cmigemo-pattern-alist-save (&optional clear)
  "Save cmigemo alist file."
  (interactive)
  (when (and cmigemo-use-pattern-alist
	     cmigemo-pattern-alist-file
	     (or cmigemo-pattern-alist clear))
    (let ((file (expand-file-name cmigemo-pattern-alist-file))
	  (coding-system-for-write cmigemo-coding-system))
      (when (file-writable-p file)
	(when clear
	  (setq cmigemo-pattern-alist nil))
	(when (and cmigemo-pattern-alist-length
		   (> (length cmigemo-pattern-alist) cmigemo-pattern-alist-length))
	  (setcdr (nthcdr (1- cmigemo-pattern-alist-length)
			  cmigemo-pattern-alist) nil))
	(with-temp-buffer
	  (if (fboundp 'pp)
	      (pp cmigemo-pattern-alist (current-buffer))
	    (prin1 cmigemo-pattern-alist (current-buffer)))
	  (write-region (point-min) (point-max) file nil 'nomsg))
	(setq cmigemo-pattern-alist nil)))))

(defun cmigemo-kill ()
  "Kill cmigemo process"
  (interactive)
  ;; mod-cmigemo begin
  (when *mod-cmigemo-handle*
    (cmigemo-close *mod-cmigemo-handle*)
    (setq *mod-cmigemo-handle* nil)))

(defun cmigemo-pattern-alist-clear ()
  "Clear cmigemo alist data & file."
  (interactive)
  (cmigemo-kill)
  (cmigemo-pattern-alist-save 'clear)
  (cmigemo-init))

(defun cmigemo-frequent-pattern-make (fcfile)
  "Create frequent pattern from `frequent-chars'."
  (interactive "ffrequent-chars: ")
  (cmigemo-pattern-alist-save 'clear)
  (when cmigemo-frequent-pattern-alist-file
    (cmigemo-kill)
    (cmigemo-init)
    (let ((file (expand-file-name cmigemo-frequent-pattern-alist-file))
	  (coding-system-for-write cmigemo-coding-system)
	  (file-coding-system  cmigemo-coding-system)
	  (cmigemo-use-pattern-alist nil)
	  (cmigemo-use-frequent-pattern-alist nil)
	  (cmigemo-after-conv-function (lambda (x y) y))
	  word regex)
      (setq cmigemo-frequent-pattern-alist nil)
      (with-temp-buffer
	(insert-file-contents fcfile)
	(goto-char (point-min))
	(message "Make frequently pattern...")
	(while (not (eobp))
	  (when (looking-at "^[a-z]+$")
	    (setq word (match-string 0))
	    (message "Make frequently pattern...%s" word)
	    (setq cmigemo-frequent-pattern-alist
		  (cons (cons word (cmigemo-get-pattern word))
			cmigemo-frequent-pattern-alist)))
	  (forward-line 1))
	(when (file-writable-p file)
	  (setq cmigemo-frequent-pattern-alist
		(nreverse cmigemo-frequent-pattern-alist))
	  (erase-buffer)
	  (if (fboundp 'pp)
	      (pp cmigemo-frequent-pattern-alist (current-buffer))
	    (prin1 cmigemo-frequent-pattern-alist (current-buffer)))
	  (write-region (point-min) (point-max) file nil 'nomsg)))
      (cmigemo-kill)
      (cmigemo-init)
      (message "Make frequently pattern...done"))))

(defun cmigemo-expand-pattern () "\
Expand the Romaji sequences on the left side of the cursor
into the cmigemo's regexp pattern."
  (interactive)
  (let ((pos (point)))
    (goto-char (- pos 1))
    (if (re-search-backward "[^-a-zA-Z]" (line-beginning-position) t)
	(forward-char 1)
      (beginning-of-line))
    (let* ((str (buffer-substring-no-properties (point) pos))
	   (jrpat (cmigemo-get-pattern str)))
      (delete-region (point) pos)
      (insert jrpat))))

;;
;;
;;
(advice-add 'isearch-mode :around
  (lambda (func &rest args)
    (let ((search-default-mode
           (if (and cmigemo-detect-default-mode
                    (japanese-p (detect-coding-with-language-environment
                                 (point-min) (point-max) "japanese")))
               #'cmigemo-search-pattern-get
             search-default-mode )))
      (apply func args)))
  '((name . cmigemo-isearch-mode-advice)))

(defun japanese-p (coding-system-list)
  (cond ((atom coding-system-list) nil)
	((member (car coding-system-list)
		 coding-system-for-japanese) t)
	(t (japanese-p (cdr coding-system-list)))))

(add-hook 'isearch-mode-hook
  (lambda ()
    (setq cmigemo-search-pattern nil
          cmigemo-search-pattern-alist nil)))

(add-hook 'isearch-mode-end-hook
  (lambda ()
    (setq cmigemo-search-pattern nil
          cmigemo-search-pattern-alist nil)))

;; experimental
(define-key global-map (kbd "C-M-;") #'cmigemo-dabbrev-expand)

(defcustom cmigemo-dabbrev-display-message nil
  "*Display dabbrev message to minibuffer."
  :group 'cmigemo
  :type 'boolean)

(defcustom cmigemo-dabbrev-ol-face 'highlight
  "*Face of cmigemo-dabbrev overlay."
  :group 'cmigemo
  :type 'face)

(defvar cmigemo-dabbrev-pattern nil)
(defvar cmigemo-dabbrev-start-point nil)
(defvar cmigemo-dabbrev-search-point nil)
(defvar cmigemo-dabbrev-pre-patterns nil)
(defvar cmigemo-dabbrev-ol nil)
(defun cmigemo-dabbrev-expand-done ()
  (remove-hook 'pre-command-hook 'cmigemo-dabbrev-expand-done)
  (unless (eq last-command this-command)
      (setq cmigemo-search-pattern-alist nil)
      (setq cmigemo-dabbrev-pre-patterns nil))
  (when cmigemo-dabbrev-ol
    (delete-overlay cmigemo-dabbrev-ol)))

(defun cmigemo-forward (word &optional bound noerror count)
  (interactive "sSearch: \nP\nP")
  (setq cmigemo-search-pattern
        (if (delq 'ascii (find-charset-string word))
            word
          (cmigemo-search-pattern-get word)))
  (search-forward-regexp cmigemo-search-pattern bound noerror count))

(defun cmigemo-backward (word &optional bound noerror count)
  (interactive "sSearch backward: \nP\nP")
  (setq cmigemo-search-pattern
        (if (delq 'ascii (find-charset-string word))
            word
          (cmigemo-search-pattern-get word)))

  (if (null cmigemo-do-isearch)
      (search-backward-regexp cmigemo-search-pattern bound noerror count)
    (or (and (not (eq this-command 'isearch-repeat-backward))
	     (not (get-char-property (point) 'invisible (current-buffer)))
	     (or (and (looking-at cmigemo-search-pattern)
		      (match-beginning 0))
		 (and (not (eq (point) (point-min)))
		      (progn (forward-char -1)
			     (and (looking-at cmigemo-search-pattern)
				  (match-beginning 0))))))
	(search-backward-regexp cmigemo-search-pattern bound noerror count))))

(defun cmigemo-dabbrev-expand ()
  (interactive)
  (let ((end-pos (point))
	matched-start matched-string)
    (if (eq last-command this-command)
	(goto-char cmigemo-dabbrev-search-point)
      (goto-char (- end-pos 1))
      (if (re-search-backward "[^a-z-]" (line-beginning-position) t)
	  (forward-char 1)
	(beginning-of-line))
      (setq cmigemo-search-pattern-alist nil)
      (setq cmigemo-dabbrev-start-point (point))
      (setq cmigemo-dabbrev-search-point (point))
      (setq cmigemo-dabbrev-pattern
	    (buffer-substring-no-properties (point) end-pos))
      (setq cmigemo-dabbrev-pre-patterns nil))
    (if (catch 'found
	  (while (if (> cmigemo-dabbrev-search-point cmigemo-dabbrev-start-point)
		     (and (cmigemo-forward cmigemo-dabbrev-pattern (point-max) t)
			  (setq cmigemo-dabbrev-search-point (match-end 0)))
		   (if (cmigemo-backward cmigemo-dabbrev-pattern (point-min) t)
		       (setq cmigemo-dabbrev-search-point (match-beginning 0))
		     (goto-char cmigemo-dabbrev-start-point)
		     (forward-word 1)
		     (message (format "Trun back for `%s'" cmigemo-dabbrev-pattern))
		     (and (cmigemo-forward cmigemo-dabbrev-pattern (point-max) t)
			  (setq cmigemo-dabbrev-search-point (match-end 0)))))
	    (setq matched-start (match-beginning 0))
	    (unless (re-search-forward ".\\>" (line-end-position) t)
	      (end-of-line))
	    (setq matched-string (buffer-substring-no-properties matched-start (point)))
	    (unless (member matched-string cmigemo-dabbrev-pre-patterns)
	      (let ((matched-end (point))
		    (str (copy-sequence matched-string))
		    lstart lend)
		(if (and (pos-visible-in-window-p matched-start)
			 (pos-visible-in-window-p matched-end))
		    (progn
		      (if cmigemo-dabbrev-ol
			  (move-overlay cmigemo-dabbrev-ol matched-start (point))
			(setq cmigemo-dabbrev-ol (make-overlay matched-start (point))))
		      (overlay-put cmigemo-dabbrev-ol 'evaporate t)
		      (overlay-put cmigemo-dabbrev-ol 'face cmigemo-dabbrev-ol-face))
		  (when cmigemo-dabbrev-ol
		    (delete-overlay cmigemo-dabbrev-ol))
		  (when cmigemo-dabbrev-display-message
		    (save-excursion
		      (save-restriction
			(goto-char matched-start)
			(setq lstart (progn (beginning-of-line) (point)))
			(setq lend (progn (end-of-line) (point)))
                        (put-text-property 0 (length str)
                                           'face cmigemo-dabbrev-ol-face str)
			(message "(%d): %s%s%s"
				 (count-lines (point-min) matched-start)
				 (buffer-substring-no-properties lstart matched-start)
				 str
				 (buffer-substring-no-properties matched-end lend)))))))
	      (throw 'found t))
	    (goto-char cmigemo-dabbrev-search-point)))
	(progn
	  (setq cmigemo-dabbrev-pre-patterns
		(cons matched-string cmigemo-dabbrev-pre-patterns))
	  (delete-region cmigemo-dabbrev-start-point end-pos)
	  (forward-char 1)
	  (goto-char cmigemo-dabbrev-start-point)
	  (insert matched-string))
      (goto-char end-pos)
      (message (format "No dynamic expansion for `%s' found"
		       cmigemo-dabbrev-pattern)))
    (add-hook 'pre-command-hook 'cmigemo-dabbrev-expand-done)))

(add-hook 'kill-emacs-hook 'cmigemo-pattern-alist-save)

(provide 'cmigemo)

;; sample
;; 0123 abcd ABCD ひらがな カタカナ 漢字 !"[#\$]%^&_':`(;)<*=+>,?-@./{|}~
