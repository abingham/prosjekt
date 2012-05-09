;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; prosjekt.el --- a software project tool for emacs
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/prosjekt
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2012 Austin Bingham
;;
;; Description:
;;
;; prosjekt is a simple software project management tool. A project
;; in prosjekt comprises 1) a top-level directory, 2) a collection
;; of files belonging to the project, and 3) a set of commands that
;; can be executed.
;;
;; For more details, see the project page at
;; https://github.com/abingham/prosjekt.
;;
;; Installation:
;;
;; Copy prosjekt.el to some location in your emacs load path. Then add
;; "(require 'prosjekt)" to your emacs initialization (.emacs,
;; init.el, or something). 
;; 
;; Installation (anything integration):
;; 
;; Prosjekt comes with integration with anything
;; (http://emacswiki.org/emacs/Anything). To enable this, copy
;; anything-prosjekt.el to your emacs load path. Then add "(require
;; 'anything-prosjekt)" to you emacs initialization. This provides the
;; anything sources "anything-c-source-prosjekt-files" and
;; "anything-c-source-prosjekt-projects".
;;
;; Example config:
;; 
;;   (require 'prosjekt)
;;   (require 'anything-prosjekt)
;; 
;;   (require 'anything)
;;   (add-to-list 'anything-sources 'anything-c-source-prosjekt-files t)
;;   (add-to-list 'anything-sources 'anything-c-source-prosjekt-projects t)
;; 
;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public API

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
  (setq prsj-proj-dir directory)
  (setq prsj-proj (prsj-default-project name))
  (prsj-setkeys (prsj-get-project-item "tools"))

  ; Update the global project list
  (prsj-set-config-item
   "project-list"
   (cons
    (cons name directory)
    (prsj-get-config-item "project-list")))

  ; save the global configuration
  (prsj-save-config)
  )

;; TODO: prosjekt-delete

(defun prosjekt-open (proj)
  "Open a project named PROJ."
  (interactive
   (list
    (completing-read "Open Project: " 
		     (mapcar 'car (prsj-get-config-item "project-list")))))
  
  (let* ((projects (prsj-get-config-item "project-list"))
	 (proj_dir (cdr (assoc proj projects))))
    (setq prsj-proj-file (expand-file-name "prosjekt.cfg" proj_dir))
    (setq prsj-proj-dir proj_dir)
    (setq prsj-proj (prsj-read-object-from-file prsj-proj-file))
    (prsj-reset-keys)
    (prsj-setkeys (prsj-get-project-item "tools"))
    (prsj-set-hooks)
    (let ((curfile (prsj-get-project-item "curfile")))
      (if curfile 
	  (find-file 
	   (expand-file-name curfile prsj-proj-dir))))
    ))

(defun prosjekt-save ()
  "Save the current project."
  (interactive)
  (if prsj-proj
      (prsj-write-object-to-file
       prsj-proj
       prsj-proj-file)))
  
(defun prosjekt-close ()
  "Close the current project."
  (interactive)
  (prosjekt-save)
  (setq prsj-proj nil)
  (setq prsj-proj-file nil)
  (setq prsj-proj-dir nil)
  (prsj-reset-keys)
  (prsj-clear-hooks)
  )

; TODO: Normalize the error messages. "No project open." everywhere,
; or whatever.
(defun prosjekt-clear ()
  "Remove all files from the current project."
  (interactive)
  (prsj-set-project-item "files" nil))

(defun prosjekt-setup ()
  "Edit the project configuration in a new buffer."
  (interactive)
  (unless prsj-proj (error "No current project."))
  (cond ((buffer-live-p prsj-buffer)
	 (switch-to-buffer prsj-buffer))
	(t
	 (setq prsj-buffer (get-buffer-create "*prosjekt*"))
	 (switch-to-buffer prsj-buffer)
	 (emacs-lisp-mode)

	 (let ((keymap (make-sparse-keymap)))
	   (define-key keymap [escape] 'prsj-setup-save-and-close)
	   (define-key keymap [C-escape] 'prsj-setup-save)
	   (use-local-map keymap))
	 
	 (insert (pp-to-string prsj-proj))

	 (goto-char (point-min))
	 ) ; t
	)  ; cond
  )        ; defun

(defun prosjekt-add (f)
  "Add a file to the current project."
  (interactive
   (let ((_ (unless prsj-proj (error "No project open."))))
     (list
      (read-file-name "Add file to project: " nil nil t nil))))
  
  (prsj-insert-file f))

(defun prosjekt-populate (dir p)
  "Add all files under DIR which match regex P to the project."
  (interactive
   (let ((_ (unless prsj-proj-dir (error "No project open."))))
     (list 
      (read-directory-name "Directory: " prsj-proj-dir)
      (read-string "Pattern: " ""))))

  (when p
    (prsj-walk-path 
     dir
     (lambda (dir file) (prsj-add-if p dir file)))))

(defun prosjekt-repopulate ()
  "Repopulate the project based on populate-spec."
  (interactive)
  (unless prsj-proj-dir (error "No project opened."))
  (let ((spec (prsj-get-project-item "populate-spec")))
    (unless spec (error "No populate-spec defined."))
    (prosjekt-clear)
    (while spec
                                        ; path is the current
                                        ; project-relative
                                        ; subdirectory
      (setq path (caar spec))

                                        ; patterns is the list of
                                        ; patterns to populate with
                                        ; from that path
      (setq patterns (cdar spec))
      (setq spec (cdr spec))
      (while patterns
        (setq pattern (car patterns))
        (setq patterns (cdr patterns))

                                        ; populate using the specified
                                        ; path and pattern
        (prosjekt-populate (concat prsj-proj-dir path) pattern)))))

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
  "Load the global config, assigning it to `prsj-config`."
  (let ((fname (prsj-config-file)))
    (setq
     prsj-config
     (if (file-exists-p fname)
	 (prsj-read-object-from-file 
	  (prsj-config-file))
       (prsj-default-config)))))

(defun prsj-save-config ()
  "Save the global config (`prsj-config`) to file."
  (prsj-write-object-to-file
   prsj-config
   (prsj-config-file)))

(defun prsj-default-config ()
  '(("version" . 0.1)
    ("project-list")
    ("last-open")))

(defun prsj-default-project (name)
  '(("name" . name)
    ("tools" ("[f5]" "emacs" compile))
    ("files")
    ("curfile" . nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks

(defun prsj-set-hooks ()
  (add-hook 'find-file-hook 'prsj-find-file-hook))

(defun prsj-clear-hooks ()
  (remove-hook 'find-file-hook 'prsj-find-file-hook))

(defun prsj-find-file-hook ()
  (let* ((abs_fname (buffer-file-name (current-buffer)))
	 (rel_fname (file-relative-name abs_fname prsj-proj-dir)))
    (if (member rel_fname (prsj-proj-files))
	(prsj-set-project-item "curfile" rel_fname))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; active-project related stuff.

(defvar prsj-buffer nil
  "The buffer for prosjekt editing tasks.")

(defvar prsj-proj nil
  "The current project definition.")

(defvar prsj-proj-file nil
  "The filename of the current project.")

(defvar prsj-proj-dir nil
  "The directory of the current project.")

(defun prsj-get-project-item (name)
  (unless prsj-proj (error "No current project."))
  (cdr (assoc name prsj-proj)))

(defun prsj-set-project-item (name val)
  (unless prsj-proj (error "No current project."))
  (setcdr (assoc name prsj-proj) val))

(defun prsj-setup-save ()
  "Save the prsj-buffer contents and the new project definition."
  (interactive) ; this is needed because we bind this method to a key.
  (unless prsj-buffer (error "No edit in progress."))
  (unless prsj-proj-file (error "No current project."))
  (switch-to-buffer prsj-buffer)
  (setq prsj-proj (read (buffer-string)))
  (minibuffer-message "New configuration enabled.")

  ; Update key bindings with edits
  ; TODO: Other edits to take care of?
  (prsj-setkeys (prsj-get-project-item "tools"))
  )

(defun prsj-setup-save-and-close () 
  "Save the prsj-buffer contents and the new project definition,
and kill that buffer."
  (interactive) ; this is needed because we bind this method to a key.
  (prsj-setup-save)
  (kill-buffer prsj-buffer)
  (setq prsj-buffer nil)
  )

(defun prsj-proj-files ()
  "Get the list of files in the active project."
  (if prsj-proj
      (mapcar 'car (prsj-get-project-item "files"))
    (list)))

(defun prsj-insert-file (f)
  (let ((files (prsj-get-project-item "files"))
	(rel_file (file-relative-name f prsj-proj-dir)))
    (unless (assoc rel_file files)
      (prsj-set-project-item 
       "files"
       (cons (list rel_file 0) files)))))

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
     (insert (pp-to-string object))
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
  "pushd to prsj-proj-dir, execute CMD, and popd."
  (let ((b (current-buffer))
	(old-dir default-directory))
    (when prsj-proj-dir (cd prsj-proj-dir))
    (cmd)
    (with-current-buffer b (cd old-dir))))

(defun prsj-bind-shell-command (key command keymap)
  "Bind KEY to execute the shell command COMMAND in KEYMAP."
  (lexical-let ((command command))
    (define-key 
      keymap 
      key 
      (lambda () (interactive) (prsj-run-tool (compile command))))))

(defun prsj-bind-interactive-function (key command keymap)
  (lexical-let ((command command))
    (define-key keymap key command)))

(defun prsj-bind-function (key command keymap)
  (lexical-let ((command command))
    (define-key
      keymap
      key
      (lambda () (interactive) (eval command)))))

(defun prsj-setkeys (bindings)
  "Set a series of bindings in the minor mode.
``bindings`` is an alist if (keycode type command)."

  (let ((keymap (cdr (prsj-get-mode-map))))
    (dolist (b bindings)
      (let ((key (read (car b)))
	    (type (cadr b))
	    (command (caddr b)))
	(cond 
	 ((equal type "interactive")
	  (prsj-bind-interactive-function key command keymap))
	 
	 ((equal type "call")
	  (prsj-bind-function key command keymap))
	 
	 ((equal type "shell")
	  (prsj-bind-shell-command key command keymap))
	)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Populate support

; TODO: Add support for skipping common "untracked" directories,
; e.g. .hg, .svn, etc.

; TODO: Some sort of auto-populate? Picks up everything that matches
; common source-file extensions

(defun prsj-walk-path (dir action)
  "walk DIR executing ACTION with (dir file)"
  (cond ((file-directory-p dir)
	 (or (char-equal ?/ (aref dir(1- (length dir))))
	     (setq dir (file-name-as-directory dir)))
	 (let ((lst (directory-files dir nil nil t))
	       fullname file)
	   (while lst
	     (setq file (car lst))
	     (setq lst (cdr lst))
	     (cond ((member file '("." "..")))
		   (t
		    (and (funcall action dir file)
			 (setq fullname (concat dir file))
			 (file-directory-p fullname)
			 (prsj-walk-path fullname action)))))))
	(t
	 (funcall action
		  (file-name-directory dir)
		  (file-name-nondirectory dir)))))

(defun prsj-add-if (p dir file)
  "If FILE matches the regex P, DIR/FILE is added to the project."
  (if (string-match p file)
      (prsj-insert-file (concat dir file))
      't))

;;;###autoload(require 'prosjekt)
(provide 'prosjekt)

(prosjekt-startup)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; prosjekt.el ends here
