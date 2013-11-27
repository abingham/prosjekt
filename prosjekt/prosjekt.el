;;; prosjekt.el --- a software project tool for emacs
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.3
;; URL: https://github.com/abingham/prosjekt
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2012 Austin Bingham
;;
;;; Commentary:
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
;; Tool descriptions: 
;;
;; The ":tools" section of a project defines commands which are
;; associated with the project. Each tool has a name, a function run
;; for the tool, an optional sequence of keybindings. A tool description looks like this:
;;   ((:name . "name of tool")
;;    (:command ...tool function...))
;;    (:keys ...list of keybinding...))
;;
;; for example:
;;
;;   ((:name . "git status")
;;    (:command git-status)
;;    (:keys "[f5]"))
;;
;;; License:
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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLIC API                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; minor-mode-map-alist entry
(defvar prosjekt-mode t)

(defvar prosjekt-open-hooks '()
  "Hooks run after any project is opened.")

(defvar prosjekt-close-hooks '()
  "Hooks run before any project is closed.")

(defvar prosjekt-ignore-dirs '(".svn" ".git" ".hg" ".bzr" ".cvs")
  "Directories which are ignored when populating projects.")

(defun prosjekt-startup ()
  "Initialize the global configuration information."
  (prosjekt-cfg-load))

(defun prosjekt-new (directory name)
  "Create a new project."
  (interactive
   (list
    (read-directory-name "Create project in directory: ")
    (read-string "Project name: ")))
  (let ((proj-list (prosjekt-cfg-project-list)))
    (if (member directory (mapcar 'cdr proj-list))
	(error "A project already exists in that directory."))
    (if (member name (mapcar 'car proj-list))
	(error "A project with that name already exists"))

    (prosjekt-close)

					; Write the new project file
    (prosjekt-write-object-to-file
     (prosjekt-default-project)
     (expand-file-name "prosjekt.cfg" directory))

					; Update the global project list
    (prosjekt-cfg-add-project name directory)

					; save the global configuration
    (prosjekt-cfg-save)

					; Load it like normal
    (prosjekt-open name)
    ))

(defun prosjekt-delete (name)
  "Delete an existing project."
  (interactive
   (list
    (completing-read "Delete project: "
		 (mapcar 'car (prosjekt-cfg-project-list)))))

  (let ((proj-list (prosjekt-cfg-project-list)))
    ; First, close the current project if it's the one being deleted.
    (ignore-errors
      (if (equal (prosjekt-proj-name) name)
	  (prosjekt-close)))

    ; Update the global project list
    (prosjekt-cfg-remove-project name)

    ; save the global configuration
    (prosjekt-cfg-save)))

(defun prosjekt-open (proj)
  "Open a project named PROJ."
  (interactive
   (list
    (completing-read "Open project: " 
		     (mapcar 'car (prosjekt-cfg-project-list)))))
  
  (let* ((projects (prosjekt-cfg-project-list))
	 (proj_dir (cdr (assoc proj projects))))
    (prosjekt-close)
    (setq prosjekt-proj-file (expand-file-name "prosjekt.cfg" proj_dir))
    (setq prosjekt-proj-dir (file-name-as-directory proj_dir))
    (setq prosjekt-proj (prosjekt-read-object-from-file prosjekt-proj-file))
    (prosjekt-setkeys (prosjekt-proj-tools))
    (prosjekt-set-hooks)
    (let ((curfile (prosjekt-proj-curfile)))
      (if curfile 
	  (find-file 
	   (expand-file-name curfile prosjekt-proj-dir))))
    (mapc 'funcall prosjekt-open-hooks)
    (mapc 'funcall (prosjekt-proj-open-hooks))
    ))

(defun prosjekt-clone (directory name clone_from)
  "Clone a new project from an existing project."
  (interactive
   (list
    (read-directory-name 
     "Create project in directory: ")
    (read-string 
     "Project name: ")
    (completing-read 
     "Clone from existing project: " 
     (mapcar 'car (prosjekt-cfg-project-list)))))

  (let* ((projects (prosjekt-cfg-project-list))
	 (clone_proj_dir (cdr (assoc clone_from projects)))
	 (clone_proj_file (expand-file-name "prosjekt.cfg" clone_proj_dir))
	 (proj (prosjekt-read-object-from-file clone_proj_file)))

    ; close any existing project
    (prosjekt-close)

    ; activate the new project specification
    (setq prosjekt-proj-file (expand-file-name "prosjekt.cfg" directory))
    (setq prosjekt-proj-dir directory)
    (setq prosjekt-proj proj)

    ; Activate the keybindings for the new project
    (prosjekt-setkeys (prosjekt-proj-tools))
    
    ; Update the global project list
    (prosjekt-cfg-add-project name directory)
    
    ; save the global configuration
    (prosjekt-cfg-save)))
    
(defun prosjekt-save ()
  "Save the current project."
  (interactive)
  (if prosjekt-proj
      (prosjekt-write-object-to-file
       prosjekt-proj
       prosjekt-proj-file)))

(defmacro defun-autosave (name args &rest body)
  "Define a function which automatically calls 'prosjekt-save at
the end"
  `(defun ,name ,args ,@body (prosjekt-save)))
  
(defun prosjekt-close ()
  "Close the current project."
  (interactive)

  ; Run global close hooks
  (mapc 'funcall prosjekt-close-hooks)

  ; Run project close hooks if there's an active project.
  (if prosjekt-proj
      (mapc 'funcall (prosjekt-proj-close-hooks)))

  (prosjekt-save)
  (setq prosjekt-proj nil)
  (setq prosjekt-proj-file nil)
  (setq prosjekt-proj-dir nil)
  (prosjekt-reset-keys)
  (prosjekt-clear-hooks)
  )

; TODO: Normalize the error messages. "No project open." everywhere,
; or whatever.
(defun-autosave prosjekt-clear ()
  "Remove all files from the current project."
  (interactive)
  (prosjekt-proj-clear-file-hash))

(defun prosjekt-setup ()
  "Edit the project configuration in a new buffer."
  (interactive)
  (unless prosjekt-proj (error "No current project."))
  (cond ((buffer-live-p prosjekt-buffer)
	 (switch-to-buffer prosjekt-buffer))
	(t
	 (setq prosjekt-buffer (get-buffer-create "*prosjekt*"))
	 (switch-to-buffer prosjekt-buffer)
	 (emacs-lisp-mode)

	 (let ((keymap (make-sparse-keymap)))
	   (define-key keymap [escape] 'prosjekt-setup-save-and-close)
	   (define-key keymap [C-escape] 'prosjekt-setup-save)
	   (use-local-map keymap))
	 
	 (insert 
	  (pp-to-string 
	   (reduce 
	    (lambda (seq key) (assq-delete-all key seq))
	    prosjekt-private-fields 
	    :initial-value (copy-alist prosjekt-proj))))

	 (goto-char (point-min))
	 ) ; t
	)  ; cond
  )        ; defun

(defun-autosave prosjekt-add (f)
  "Add a file to the current project."
  (interactive
   (let ((_ (unless prosjekt-proj (error "No project open."))))
     (list
      (read-file-name "Add file to project: " nil nil t nil))))
  
  (prosjekt-insert-file f))

(defun-autosave prosjekt-populate (dir p)
  "Add all files under DIR which match regex P to the project."
  (interactive
   (let ((_ (unless prosjekt-proj-dir (error "No project open."))))
     (list 
      (read-directory-name "Directory: " prosjekt-proj-dir)
      (read-string "Pattern: " ""))))

  (when p
    (prosjekt-walk-path 
     dir
     (lambda (dir file) (prosjekt-add-if p dir file)))))

(defun prosjekt-repopulate ()
  "Repopulate the project based on populate-spec."
  (interactive)
  (unless prosjekt-proj-dir (error "No project opened."))
  (let ((spec (prosjekt-proj-populate-spec)))
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
        (prosjekt-populate (concat prosjekt-proj-dir path) pattern)))))

(defun prosjekt-run-tool-by-name (name)
  (interactive
   (list
    (completing-read "Command name: "
                     (prosjekt-tool-names))))
  (let ((tool (prosjekt-find-tool-by-name name)))
    (if tool
        (let* ((default-directory (or prosjekt-proj-dir default-directory))
               (command (cdr (assoc :command tool)))
               (is-interactive (interactive-form command)))
          (if is-interactive
              (call-interactively command)
            (eval command))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; active-project related stuff.

(defvar prosjekt-proj nil
  "The current project definition.")

(defvar prosjekt-proj-dir nil
  "The directory of the current project.")

(defun prosjekt-proj-check ()
  "Check if a project is currently open, throwing an error if not."
  (unless prosjekt-proj 
    (error "No project is open.")))

(defun prosjekt-proj-get-item_ (name)
  (prosjekt-proj-check)
  (cdr (assoc name prosjekt-proj)))

(defun prosjekt-proj-set-item_ (name val)
  (prosjekt-proj-check)
  (setcdr (assoc name prosjekt-proj) val))

(defun prosjekt-proj-tools ()
  (prosjekt-proj-get-item_ :tools))

(defun prosjekt-proj-curfile ()
  (prosjekt-proj-get-item_ :curfile))

(defun prosjekt-proj-set-curfile (fname)
  (prosjekt-proj-set-item_ :curfile fname))

(defun prosjekt-proj-open-hooks ()
  (prosjekt-proj-get-item_ :open-hooks))

(defun prosjekt-proj-close-hooks ()
  (prosjekt-proj-get-item_ :close-hooks))

(defun prosjekt-proj-populate-spec ()
  (prosjekt-proj-get-item_ :populate-spec))

(defun prosjekt-proj-file-hash ()
  (prosjekt-proj-get-item_ :files))

(defun prosjekt-proj-clear-file-hash ()
  (prosjekt-proj-set-item_ 
   :files 
   (make-hash-table :test 'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION DETAILS: Users should not generally need to call or look    ;;
;; below here.                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global config-related functionality

(defvar prosjekt-config nil 
  "The global prosjekt configuration.")

(defun prosjekt-cfg-file ()
  "Get the global configuration filename (~/.emacs.d/prosjekt.lst)"
  (expand-file-name 
   "prosjekt.lst"
   (if (boundp 'user-emacs-directory) 
       user-emacs-directory
     "~/.emacs.d/"
     )))

(defun prosjekt-cfg-project-list ()
  (cdr (assoc :project-list prosjekt-config)))

(defun prosjekt-cfg-add-project (name dir)
  (setcdr (assoc :project-list prosjekt-config)
	  (cons
	   (cons name dir)
	   (prosjekt-cfg-project-list))))

(defun prosjekt-cfg-remove-project (name)
  (setcdr (assoc :project-list prosjekt-config)
	  (remove* name
		   (prosjekt-cfg-project-list)
		   :test 'equal
		   :key 'car)))

(defun prosjekt-cfg-load ()
  "Load the global config, assigning it to `prosjekt-config`."
  (let ((fname (prosjekt-cfg-file)))
    (setq
     prosjekt-config
     (if (file-exists-p fname)
	 (prosjekt-read-object-from-file 
	  (prosjekt-cfg-file))
       (prosjekt-cfg-default)))))

(defun prosjekt-cfg-save ()
  "Save the global config (`prosjekt-config`) to file."
  (prosjekt-write-object-to-file
   prosjekt-config
   (prosjekt-cfg-file)))

(defun prosjekt-cfg-default ()
  '((:version . 1)
    (:project-list)
    (:last-open)))

(defun prosjekt-default-project ()
  (let ((files (make-hash-table :test 'equal)))
    (list
     '(:tools 
       ((:name . "compile") 
	(:command . compile) 
	(:keys "[f5]")))
     (cons :files files)
     '(:curfile . nil)
     '(:populate-spec)
     '(:open-hooks)
     '(:close-hooks)
     '(:version . 1)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attach prosjekt into other hooks.

(defun prosjekt-set-hooks ()
  (add-hook 'find-file-hook 'prosjekt-find-file-hook))

(defun prosjekt-clear-hooks ()
  (remove-hook 'find-file-hook 'prosjekt-find-file-hook))

(defun prosjekt-find-file-hook ()
  (let* ((abs_fname (buffer-file-name (current-buffer)))
	 (rel_fname (file-relative-name abs_fname prosjekt-proj-dir)))
    (if (gethash rel_fname (prosjekt-proj-file-hash))
	(prosjekt-proj-set-curfile rel_fname))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; active-project related stuff.

(defvar prosjekt-private-fields '(:curfile :files :version)
  "Project fields which are not displayed in the setup buffer.")

(defvar prosjekt-buffer nil
  "The buffer for prosjekt editing tasks.")

(defvar prosjekt-proj-file nil
  "The filename of the current project.")

(defun-autosave prosjekt-setup-save ()
  "Save the prosjekt-buffer contents and the new project definition."
  (interactive) ; this is needed because we bind this method to a key.
  (unless prosjekt-buffer (error "No edit in progress."))
  (unless prosjekt-proj-file (error "No current project."))
  (switch-to-buffer prosjekt-buffer)
  (let ((new-proj (read-from-whole-string (buffer-string))))
    (setq prosjekt-proj
	  (reduce (lambda (seq key)
                    (let ((private_value (assoc key prosjekt-proj)))
                      (if private_value
                          (cons (assoc key prosjekt-proj) seq)
                        seq)))
		  prosjekt-private-fields
		  :initial-value new-proj)))
  (minibuffer-message "New configuration enabled.")

  ; Update key bindings with edits
  ; TODO: Other edits to take care of?
  (prosjekt-setkeys (prosjekt-proj-tools)))

(defun prosjekt-setup-save-and-close () 
  "Save the prosjekt-buffer contents and the new project definition,
and kill that buffer."
  (interactive) ; this is needed because we bind this method to a key.
  (prosjekt-setup-save)
  (kill-buffer prosjekt-buffer)
  (setq prosjekt-buffer nil)
  )

(defun prosjekt-proj-files ()
  "Get the list of files in the active project."
  (if prosjekt-proj
      (prosjekt-hash-keys (prosjekt-proj-file-hash))))

(defun prosjekt-insert-file (f)
  (let ((files (prosjekt-proj-file-hash))
	(rel_file (file-relative-name f prosjekt-proj-dir)))
    (unless (gethash rel_file files)
      (puthash rel_file 0 files))))

(defun prosjekt-proj-name ()
  "Get the name of the current project."
  (unless prosjekt-proj-dir (error "No current project."))
  (let ((proj-list (prosjekt-alist-transpose (prosjekt-cfg-project-list))))
    (cdr (assoc prosjekt-proj-dir proj-list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for reading elisp code from files

;; TODO: Are there better, standard versions of these functions?
(defun prosjekt-read-object-from-file (filename)
  "Read FILENAME's complete contents and 'read' them as a lisp
  object."
  (with-temp-buffer
    (insert-file-contents filename)
    (read (buffer-string))))

(defun prosjekt-write-object-to-file (object filename)
  "Write STRING as the contents of FILENAME."
   (with-temp-buffer
     (insert (pp-to-string object))
     (when (file-writable-p filename)
       (write-file filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for dealing with the keymap and bindings.

(defun prosjekt-get-mode-map ()
  "Get the (mode . keymap) cell from minor-mode-map-alist.
This will initialize the entry if needed."
  (let ((m (assoc 'prosjekt-mode minor-mode-map-alist)))
    (or	m
	(let ((mode (cons 'prosjekt-mode (make-sparse-keymap))))
	  (car (push mode minor-mode-map-alist))))))

(defun prosjekt-reset-keys ()
  "Clear the keybindings for the minor mode."
  (setcdr (prosjekt-get-mode-map) (make-sparse-keymap)))

(defun prosjekt-setkeys (tools)
  "Set a series of tools in the minor mode.
TOOLS is a list of keybinding descriptions."
  (let ((keymap (cdr (prosjekt-get-mode-map))))
    (dolist (tool tools)
      (let* ((keys (cdr (assoc :keys tool)))
	     (command (cdr (assoc :command tool)))
	     (is-interactive (interactive-form command)))
	(dolist (key keys)
	  (let ((key (read key)))
	    (lexical-let ((command command)
			  (is-interactive is-interactive))
	      (define-key keymap key
		(lambda ()
		  (interactive)
		  (let ((default-directory (or prosjekt-proj-dir default-directory)))
		    (if is-interactive
			(call-interactively command)
		      (eval command))))))))))))

(defun prosjekt-tool-names ()
  (let ((tools (prosjekt-proj-tools)))
    (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)))

(defun prosjekt-find-tool-by-name (name)
  (let ((tools (prosjekt-proj-tools)))
    (car 
     (remove-if-not 
      (lambda (tool) 
	(string= (cdr (assoc :name tool)) name)) 
      tools))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Populate support

; TODO: Add support for skipping common "untracked" directories,
; e.g. .hg, .svn, etc.

; TODO: Some sort of auto-populate? Picks up everything that matches
; common source-file extensions

(defun prosjekt-walk-path (dir action)
  "walk DIR executing ACTION with (dir file)"
  (cond ((file-directory-p dir)
	 (or (char-equal ?/ (aref dir(1- (length dir))))
	     (setq dir (file-name-as-directory dir)))
	 (let ((lst (directory-files dir nil nil t))
	       fullname file)
	   (while lst
	     (setq file (car lst))
	     (setq lst (cdr lst))
	     (cond ((or (member file '("." ".."))
			(member file prosjekt-ignore-dirs)))
		   (t
		    (and (funcall action dir file)
			 (setq fullname (concat dir file))
			 (file-directory-p fullname)
			 (prosjekt-walk-path fullname action)))))))
	(t
	 (funcall action
		  (file-name-directory dir)
		  (file-name-nondirectory dir)))))

(defun prosjekt-add-if (p dir file)
  "If FILE matches the regex P, DIR/FILE is added to the project."
  (if (string-match p file)
      (prosjekt-insert-file (concat dir file))
      't))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other misc. functions

(defun prosjekt-hash-keys (h)
  "Get the list keys from hash-table H."
  (let (keys)
    (maphash (lambda (k v) (setq keys (cons k keys))) h)
    keys))

(defun prosjekt-alist-transpose (a)
  (mapcar (lambda (x) (cons (cdr x) (car x))) a))

; Add the "ext" directory to the load path. This makes it more
; convenient for users to load extensions.
(if load-file-name
    (add-to-list 'load-path
		 (concat
		  (file-name-directory load-file-name)
		  "/ext")))

;;;###autoload(require 'prosjekt)
(provide 'prosjekt)

(prosjekt-startup)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; prosjekt.el ends here
