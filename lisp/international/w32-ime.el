;;;;; w32-ime.el ---- Meadow features for NTEmacs.
;;
;;   Author H.Miyashita
;;
;;;;;

(defgroup W32-IME nil
  "w32-ime"
  :group 'emacs)

;----------

;; IME state indicator
;;
(global-set-key [kanji] 'ignore)
(global-set-key [M-kanji] 'ignore) ; for 101
(global-set-key [compend] 'ignore)

(defcustom w32-ime-on-hook nil
  "Functions to eval when IME is turned on at least.
Even if IME state is not changed, these functiona are maybe called."
  :version "24.5"
  :type 'boolean
  :group 'W32-IME)
(defcustom w32-ime-buffer-switch-p t
  "If this variable is nil, IME control when buffer is switched is disabled."
  :version "24.5"
  :type 'boolean
  :group 'W32-IME)
(defcustom w32-ime-show-mode-line t
  "When t, mode line indicates IME state."
  :version "24.5"
  :type 'boolean
  :group 'W32-IME)

(defcustom w32-ime-off-hook nil
  "Functions to eval when IME is turned off at least.
Even if IME state is not changed, these functiona are maybe called.")
(defvar w32-ime-mode-line-state-indicator "[O]"
  "This is shown at the mode line. It is regarded as state of ime.")

(make-variable-buffer-local 'w32-ime-mode-line-state-indicator)
(put 'w32-ime-mode-line-state-indicator 'permanent-local t)

(defvar w32-ime-mode-line-state-indicator-list '("-" "[|]" "[O]")
  "List of IME state indicator string.")
(defvar w32-ime-mode-line-format-original nil
  "Original mode line format.")
;;
;;
(defun w32-ime-sync-state (window)
  (when w32-ime-buffer-switch-p
    (with-current-buffer (window-buffer window)
      (let ((ime-state (ime-get-open-status)))
	(cond
	 ((and (not ime-state)
	       (equal current-input-method "W32-IME"))
	  (ime-set-open-status t)
	  (run-hooks 'w32-ime-on-hook))
	 ((and ime-state
	       (not (equal current-input-method "W32-IME")))
	  (ime-set-open-status nil)
	  (run-hooks 'w32-ime-off-hook)))))))

(defun w32-ime-set-selected-window-buffer-hook (oldbuf newwin newbuf)
  (w32-ime-sync-state newwin))
(defun w32-ime-select-window-hook (old new)
  (w32-ime-sync-state new))
(defun w32-ime-w32-ime-set-open-status-hook (state)
  (activate-input-method (and state "W32-IME")))

(defun w32-ime-mode-line-update ()
  (cond
   (w32-ime-show-mode-line
    (unless (window-minibuffer-p (selected-window))
      (setq w32-ime-mode-line-state-indicator
	    (nth (if (ime-get-open-status) 1 2)
		 w32-ime-mode-line-state-indicator-list))))
   (t
    (setq w32-ime-mode-line-state-indicator
	  (nth 0 w32-ime-mode-line-state-indicator-list))))
  (force-mode-line-update))

(defun w32-ime-init-mode-line-display ()
  (unless (member 'w32-ime-mode-line-state-indicator mode-line-format)
    (setq w32-ime-mode-line-format-original
	  (default-value 'mode-line-format))
    (if (and (stringp (car mode-line-format))
	     (string= (car mode-line-format) "-"))
	(setq-default mode-line-format
		      (cons ""
			    (cons 'w32-ime-mode-line-state-indicator
				  (cdr mode-line-format))))
      (setq-default mode-line-format
		    (cons ""
			  (cons 'w32-ime-mode-line-state-indicator
				mode-line-format))))
    (force-mode-line-update t)))

(defun w32-ime-initialize ()
   (when (and (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	      (eq window-system 'w32)
	      (featurep 'w32-ime))
     (w32-ime-init-mode-line-display)
     (w32-ime-mode-line-update)
     (add-hook 'select-window-functions
	       'w32-ime-select-window-hook)
     (add-hook 'set-selected-window-buffer-functions
	       'w32-ime-set-selected-window-buffer-hook)
     (add-hook 'w32-ime-set-open-status-functions
	       'w32-ime-w32-ime-set-open-status-hook)
     (define-key global-map [kanji] 'toggle-input-method)))  ; C-\

(defun w32-ime-uninitialize ()
  (when (and (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
	     (eq window-system 'w32)
	     (featurep 'w32-ime))
    (setq-default mode-line-format
		  w32-ime-mode-line-format-original)
    (force-mode-line-update t)
    (remove-hook 'select-window-functions
		 'w32-ime-select-window-hook)
    (remove-hook 'set-selected-window-buffer-functions
		 'w32-ime-set-selected-window-buffer-hook)
    (remote-hook 'w32-ime-set-open-status-functions
		 'w32-ime-w32-ime-set-open-status-hook)
    (define-key global-map [kanji] 'ignore)))

(defun w32-ime-exit-from-minibuffer ()
  (deactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'w32-ime-exit-from-minibuffer)))

(defun w32-ime-state-switch (&optional arg)
  (if arg
      (progn
	(setq deactivate-current-input-method-function
	      'w32-ime-state-switch)
	(run-hooks 'input-method-activate-hook)
	(run-hooks 'w32-ime-on-hook)
	(setq describe-current-input-method-function nil)
	(when (eq (selected-window) (minibuffer-window))
	  (add-hook 'minibuffer-exit-hook 'w32-ime-exit-from-minibuffer))
	(ime-set-open-status t))
    (setq current-input-method nil)
    (run-hooks 'input-method-deactivate-hook)
    (run-hooks 'w32-ime-off-hook)
    (setq describe-current-input-method-function nil)
    (ime-set-open-status nil))
  (w32-ime-mode-line-update))

(register-input-method "W32-IME" "Japanese" 'w32-ime-state-switch ""
		       "W32 System IME")

(provide 'w32-ime)
