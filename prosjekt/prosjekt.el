
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; prosjekt.el --- project workspaces for emacs
;;
;; Copyright (C) 2012 Austin Bingham
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; minor-mode-map-alist entry
(defvar prosjekt-mode t)

(defvar prsj-cur-proj nil
  "The current project definition, nil if no project.")

(defun prosjekt-startup ()
  "Initialize the global configuration information."
  (prsj-load-config))

(defun prosjekt-open (proj)
  "Open a project named PROJ."

  (interactive
   (list
    (completing-read "Open Project: " 
		     (mapcar 'car (prsj-get-config-item "project-list")))))
  
  (let* ((projects (prsj-get-config-item "project-list"))
	 (proj_dir (cdr (assoc proj projects)))
	 (proj_file (expand-file-name "prosjekt.cfg" proj_dir)))
    (setq prsj-cur-proj (read (prsj-get-string-from-file proj_file)))
    (prsj-reset-keys)
    (prsj-setkeys (cdr (assoc "tools" prsj-cur-proj)))
    ; TODO: set up command key mappings
    ; TODO: open curfile if it's set
    ))

(defun prosjekt-close ()
  (interactive)
  (setq prsj-cur-proj nil)
  (prsj-reset-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global config-related functionality

(defvar prsj-config nil 
  "The global prosjekt configuration.")

(defun prsj-config-file ()
  "Get the global configuration filename (~/.emacs.d/prosjekt.lst)"
  (expand-file-name 
   "prosjekt.lst"
   (if (boundp 'user-emacs-directory) 
       user-emacs-directory
     "~/.emacs.d/"
     )))

(defun prsj-get-config-item (name)
  "Get a value from the global config."
  (cdr (assoc name prsj-config)))

(defun prsj-load-config ()
  (setq prsj-config 
	(read 
	 (prsj-get-string-from-file (prsj-config-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for reading elisp code from files

;; TODO: Are there better, standard versions of these functions?
(defun prsj-get-string-from-file (filename)
  "Return FILENAME's complete file content."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun prsj-write-string-to-file (string filename)
  "Write STRING as the contents of FILENAME."
   (with-temp-buffer
     (insert string)
     (when (file-writable-p filename)
       (write-region (point-min)
                     (point-max)
                     file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for dealing with the keymap and bindings.

(defun prsj-get-mode-map ()
  "Get the (mode . keymap) cell from minor-mode-map-alist.
This will initialize the entry if needed."
  (let ((m (assoc 'prosjekt-mode minor-mode-map-alist)))
    (or	m
	(let ((mode (cons 'prosjekt-mode (make-sparse-keymap))))
	  (car (push mode minor-mode-map-alist))))))

(defun prsj-reset-keys ()
  "Clear the keybindings for the minor mode."
  (setcdr (prsj-get-mode-map) (make-sparse-keymap)))
		   
(defun prsj-run-tool (cmd)
  (compile cmd))

(defun prsj-setkeys (bindings)
  "Set a series of bindings in the minor mode.
``bindings`` is an alist if (keycode type command)."
  (let ((keymap (cdr (prsj-get-mode-map))))
    (dolist (b bindings)
      (let ((key (read (car b)))
	    (type (cadr b))
	    (command (caddr b)))
	(cond ((equal type "emacs")
	       (define-key keymap key command))
	      ((equal type "shell")
	       (let ((fn (list 'lambda)))
		 (setcdr fn `(() (interactive) 
			      (compile ',command)))
		 (define-key keymap key fn)))
	      )
))))

;;;###autoload(require 'prosjekt)
(provide 'prosjekt)
(prosjekt-startup)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; prosjekt.el ends here
