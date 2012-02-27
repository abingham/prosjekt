
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; prosjekt.el --- project workspaces for emacs
;;
;; Copyright (C) 2012 Austin Bingham
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; minor-mode-map-alist entry
(defvar prosjekt-mode t)

(defun prosjekt-startup ()
  "Initialize the global configuration information."
  (prsj-load-config))

(defun prosjekt-new (directory name)
  "Create a new project."
  (interactive
   (list
    (read-directory-name "Create project in directory: ")
    (read-string "Project name: ")))
  ; TODO: Check for duplicate project name
  (prosjekt-close)
  (setq prsj-proj-file (expand-file-name "prosjekt.cfg" directory))
  (setq prsj-proj (prsj-default-project name))
  (prsj-setkeys (prsj-get-project-item "tools"))

  ; Update the global project list
  (prsj-set-config-item
   "project-list"
   (cons
    (cons name directory)
    (prsj-get-config-item "project-list"))))

(defun prosjekt-open (proj)
  "Open a project named PROJ."

  (interactive
   (list
    (completing-read "Open Project: " 
		     (mapcar 'car (prsj-get-config-item "project-list")))))
  
  (let* ((projects (prsj-get-config-item "project-list"))
	 (proj_dir (cdr (assoc proj projects))))
    (setq prsj-proj-file (expand-file-name "prosjekt.cfg" proj_dir))
    (setq prsj-proj (prsj-read-object-from-file prsj-proj-file))
    (prsj-reset-keys)
    (prsj-setkeys (prsj-get-project-item "tools"))
    ; TODO: open curfile if it's set
    ))

(defun prosjekt-close ()
  (interactive)
  (when prsj-proj
    (prsj-write-object-to-file 
     prsj-proj 
     prsj-proj-file))
  (setq prsj-proj nil)
  (setq prsj-proj-file nil)
  (prsj-reset-keys)
  ;; TODO: save list to ~/.emacs.d/prosjekt.lst
  )

(defun prosjekt-setup ()
  (interactive)
  (unless (boundp 'prsj-proj) (error "No current project."))
  (cond ((buffer-live-p prsj-buffer)
	 (switch-to-buffer prsj-buffer))
	(t
	 (setq prsj-buffer (get-buffer-create "*prosjekt*"))
	 (switch-to-buffer prsj-buffer)))
    
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [escape] 'prsj-setup-save)
    (use-local-map keymap))

  (cl-prettyprint prsj-proj)
  )

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

(defun prsj-set-config-item (name val)
  "Set a value in the global config."
  (setcdr (assoc name prsj-config) val))

(defun prsj-load-config ()
  "Load the global config, assiging it to `prsj-config`."
  (setq 
   prsj-config 
   (prsj-read-object-from-file 
    (prsj-config-file))))

(defun prsj-save-config ()
  "Save the global config (`prsj-config`) to file."
  (prsj-write-object-to-file
   prsj-config
   (prsj-config-file)))

(defun prsj-default-project (name)
  '(("version" . "0.1") ;; TODO: Get rid of this?
    ("config" ("name" . name))
    ("tools" ("[f5]" "emacs" compile))
    ("files")
    ("curfile" . nil)
    
    ;; TODO: Do we really need this?
    ("functions")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; active-project related stuff.

(defvar prsj-buffer nil
  "The buffer for prosjekt editing tasks.")

(defvar prsj-proj nil
  "The current project definition.")

(defvar prsj-proj-file nil
  "The filename of the current project.")

(defun prsj-get-project-item (name)
  (unless (boundp 'prsj-proj) (error "No current project."))
  (cdr (assoc name prsj-proj)))

(defun prsj-setup-save () 
  (interactive) 
  (unless (boundp 'prsj-buffer) (error "No edit in progress."))
  (unless (boundp 'prsj-proj-file) (error "No current project."))
  (switch-to-buffer prsj-buffer)
  (setq prsj-proj (read (buffer-string)))
  (kill-buffer prsj-buffer)
  (setq prsj-buffer nil)

  ; Update key bindings with edits
  ; TODO: Other edits to take care of?
  (prsj-setkeys (prsj-get-project-item "tools"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for reading elisp code from files

;; TODO: Are there better, standard versions of these functions?
(defun prsj-read-object-from-file (filename)
  "Read FILENAME's complete contents and 'read' them as a lisp
  object."
  (with-temp-buffer
    (insert-file-contents filename)
    (read (buffer-string))))

(defun prsj-write-object-to-file (object filename)
  "Write STRING as the contents of FILENAME."
   (with-temp-buffer
     (cl-prettyprint object)
     (when (file-writable-p filename)
       (write-file filename))))

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

  ;; TODO: We need to redirect the command to a function that will cd
  ;; to the correct directory before execution.

  (let ((keymap (cdr (prsj-get-mode-map))))
    (dolist (b bindings)
      (let ((key (read (car b)))
	    (type (cadr b))
	    (command (caddr b)))
	(cond ((equal type "emacs")
	       (define-key keymap key command))
	      ((equal type "shell")
	       ; This code below deals with the fact that elisp has dynamic binding. There are possibly cleaner ways to write this: http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&q=%22FakeClosures%22
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
