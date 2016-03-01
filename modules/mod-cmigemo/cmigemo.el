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
(isearch-define-mode-toggle cmigemo "m" cmigemo-get-pattern "\
Turning on cmigemo search turns off regexp mode.")

;;; Code:
(defgroup cmigemo nil
  "cmigemo - Japanese incremental search trough dynamic pattern expansion."
  :group 'matching)

(defcustom cmigemo-detect-default-mode t
  "if t, detect buffer's coding system and choose migemo or search-default-mode, "
  :group 'cmigemo
  :type 'boolean)

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

(defcustom cmigemo-isearch-min-length 1
  "*Minimum length of word to start isearch."
  :group 'cmigemo
  :type 'integer)

;; internal variables
(defvar cmigemo-buffer nil)
(defvar cmigemo-current-input-method nil)
(defvar cmigemo-search-pattern nil)
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

(defun cmigemo-replace-in-string (string from to)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let ((migemo-do-isearch nil))
      (while (search-forward from nil t)
        (replace-match to nil t)))
    (buffer-substring (point-min) (point-max))))

(defun cmigemo-get-pattern (word &optional lax)
  (if (< (length word) cmigemo-isearch-min-length)
      word
    (let ((word (if (and cmigemo-pre-conv-function
                         (functionp cmigemo-pre-conv-function))
                    (funcall cmigemo-pre-conv-function word)
                  word))
          (pattern
           (progn
             (cmigemo-init)
             (cmigemo-query *mod-cmigemo-handle* word))))
      (if (and cmigemo-after-conv-function
               (functionp cmigemo-after-conv-function))
          (funcall cmigemo-after-conv-function word pattern)
        pattern))))

(defun cmigemo-init ()
  (unless *mod-cmigemo-handle*
    (setq *mod-cmigemo-handle*
          (cmigemo-open cmigemo-dictionary))))

(defun cmigemo-kill ()
  "Kill cmigemo process"
  (interactive)
  (when *mod-cmigemo-handle*
    (cmigemo-close *mod-cmigemo-handle*)
    (setq *mod-cmigemo-handle* nil)))

(advice-add 'isearch-mode :around
  (lambda (func &rest args)
    (let ((search-default-mode
           (if (and cmigemo-detect-default-mode
                    (japanese-p (detect-coding-with-language-environment
                                 (point-min) (point-max) "japanese")))
               #'cmigemo-get-pattern
             search-default-mode )))
      (apply func args)))
  '((name . cmigemo-isearch-mode-advice)))

(defun japanese-p (coding-system-list)
  (cond ((atom coding-system-list) nil)
	((member (car coding-system-list)
		 coding-system-for-japanese) t)
	(t (japanese-p (cdr coding-system-list)))))

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
  (when cmigemo-dabbrev-ol
    (delete-overlay cmigemo-dabbrev-ol)))

(defun cmigemo-forward (word &optional bound noerror count)
  (interactive "sSearch: \nP\nP")
  (setq cmigemo-search-pattern
        (if (delq 'ascii (find-charset-string word))
            word
          (cmigemo-get-pattern word)))
  (search-forward-regexp cmigemo-search-pattern bound noerror count))

(defun cmigemo-backward (word &optional bound noerror count)
  (interactive "sSearch backward: \nP\nP")
  (setq cmigemo-search-pattern
        (if (delq 'ascii (find-charset-string word))
            word
          (cmigemo-get-pattern word)))

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
