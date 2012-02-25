
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
		   
(defun prsj-setkeys (bindings)
  "Set a series of bindings in the minor mode.
``bindings`` is an alist if (keycode.function)."
  (let ((keymap (cdr (prsj-get-mode-map))))
    (dolist (b bindings)
      (define-key keymap (read (car b)) (cdr b))
    )))

;;   ; If a is just a project name, look up the (name.dir) cell in the
;;   ; project list.
;;   (unless (consp proj)
;;     (let ((proj_in_list (assoc proj prsj-list)))
;;       (unless proj_in_list
;;         (error "No such project: %s" proj)
;;         )
;;       (setq proj proj_in_list)
;;       ))

;;   ; Get the project def from the list (in case the argument a was not
;;   ; from that list)
;;   (setq proj 
;; 	(or 
;; 	 (car (member proj prsj-list)) ; Look up in project list 
;; 	 proj))                      

;;   ; If a is not the current project, we need to close the current
;;   ; project and open a
;;   (unless (eq a prsj-current)
;;     (unless (file-directory-p (prsj-get-directory a))
;;       (error "No such directory: %s" (cadr a))
;;       )
    
;;     ; Move a to the front of the list
;;     (setq prsj-list (cons a (delq a prsj-list)))
    
;;     ; Close the current project
;;     (prosjekt-close)

;;     ; Load the project config for a
;;     (prsj-loadconfig a)
;;     )

;;   (prsj-addhooks)
;;   (prsj-setup-all)
;;   (prsj-isearch-setup)
;;   (unless (prsj-edit-file prsj-curfile)
;;     (prosjekt-dired)
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-configurable items:
;; (defgroup prsj-group '()
;;    "Group for prosjekt customization")

;; (defcustom prsj-keybindings '(
;;   ([f5]         prosjekt-setup-toggle  always)
;;   ([M-right]    prosjekt-nextfile)
;;   ([M-left]     prosjekt-prevfile)
;;   ([C-f5]       prosjekt-dired)
;;   )
;;   "Key bindings in prosjekt"
;;    :type 
;;    '(repeat 
;;       (list 
;; 	 (restricted-sexp 
;; 	    :tag "Key sequence" 
;; 	    :match-alternatives (vectorp stringp))
;; 	 function
;; 	 (choice :format "%[Always provide%] %v"
;; 	    (const :tag "yes" always)
;; 	    (const :tag "no" nil))))
;;    :group 'prsj-group)


;; (defcustom prsj-default-config 
;;    '(
;;        ("Make"       "make" "f9")
;;        ("Clean"      "make clean" "C-f9")
;;        ("Run"        "echo run what" "f8")
;;        ("Stop"       "-e prosjekt-killtool" "C-f8")
;;        ("---")
;;        ("Configure"  "./configure")
;;        ("---")
;;        ("Explore Project" "nautilus --browser `pwd` &")
;;        ("XTerm In Project" "xterm &"))
;;   "*The default tools menu for new projects in prosjekt."
;;    :type
;;    '(repeat
;;        (choice  :format "%v"
;; 	  (const :tag "--- Item separator ---" ("---"))
;; 	  (list :format "\n%v"
;; 	     (string :tag "name")
;; 	     (string :tag "shell command")
;; 	     (choice :format "%[Toggle%] %v"
;; 		(restricted-sexp 
;; 		   :tag "Key sequence"
;; 		   :match-alternatives (vectorp stringp))
;; 		(const :inline t :tag "No key sequence" nil)))))
;;    :group 'prsj-group)


;; (defcustom prsj-autotracking t
;;   "*Should prosjekt automatically add/remove files to/from the project (nil/t)"
;;    :type  'boolean
;;    :group 'prsj-group)
;;   ; To apply, close and reopen the project.

;; (defcustom prsj-rename-buffers t
;;   "*Should prosjekt rename buffers to project-relative filenames (nil/t)"
;;    :type  'boolean
;;    :group 'prsj-group)

;; (defcustom prsj-set-default-directory nil
;;   "*Should prosjekt set the project directory as default-directory
;; for all project files (nil/t)."
;;    :type  'boolean
;;    :group 'prsj-group)

;; (defcustom prsj-set-framepos nil
;;   "*Should prosjekt restore the last frame position/size (nil/t)."
;;    :type  'boolean
;;    :group 'prsj-group)

;; (defcustom prsj-set-compilation-frame nil
;;   "*Should prosjekt show compilation output in the other frame (nil/t)."
;;    :type  'boolean
;;    :group 'prsj-group)
  
;; (defcustom prsj-set-multi-isearch nil
;;   "*Should prosjekt setup multi-isearch in the project files (nil/t)."
;;    :type  'boolean
;;    :group 'prsj-group)

;; ;; End of user-configurable items
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; There is a global file (~/.emacs.d/prosjekt.lst)
;; (defun prsj-globalfile ()
;;   (expand-file-name "prosjekt.lst"
;;      (if (boundp 'user-emacs-directory) 
;;          user-emacs-directory
;;          "~/.emacs.d/"
;;          )))

;; ;; with the list of all projects
;; ;; alist of name -> directory
;; (defvar prsj-list)

;; ;; and the project that was open in the last session (if any)
;; (defvar prsj-last-open nil)

;; ;; and the frame coords from last session
;; (defvar prsj-frame-pos nil)

;; ;; prosjekt version that created the config file
;; (defvar prsj-version nil)

;; ;; Here is a function to reset these
;; (defun prsj-init ()
;;   (setq prsj-version nil)
;;   (setq prsj-list nil)
;;   (setq prsj-last-open nil)
;;   (setq prsj-frame-pos nil)
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Each project has a directory

;; (defvar prsj-directory)

;; ;; with a configuration files in it
;; (defvar prsj-default-cfg "prosjekt.cfg")

;; ;; This file defines:

;; ;; the list of files
;; (defvar prsj-files)

;; ;; the current file
;; (defvar prsj-curfile)

;; ;; an alist of settings
;; (defvar prsj-config)

;; ;; a list of tools
;; (defvar prsj-tools)

;; ;; a list of utility functions (feature incomplete)
;; (defvar prsj-functions nil)

;; ;; directory to run commands, default to prsj-directory
;; (defvar prsj-exec-directory)

;; ;; The current project
;; (defvar prsj-current)

;; ;; A list with generated functions for each tool
;; (defvar prsj-tools-fns)

;; ;; A list with files removed from the project
;; (defvar prsj-removed-files)

;; ;; Here is a function to reset/close the project
;; (defun prsj-reset ()
;;   (setq prsj-version nil)
;;   (setq prsj-current nil)
;;   (setq prsj-directory nil)
;;   (setq prsj-exec-directory nil)
;;   (setq prsj-files nil)
;;   (setq prsj-removed-files nil)
;;   (setq prsj-curfile nil)
;;   (setq prsj-config nil)
;;   (setq prsj-tools-fns nil)
;;   (setq prsj-tools (copy-tree prsj-default-config))
;;   (prsj-reset-functions)
;;   )

;; (defun prsj-reset-functions ()
;;   (dolist (l prsj-functions)
;;     (if (eq (car l) 'setq)
;;         (makunbound (cadr l))
;;       (fmakunbound (cadr l))
;;       ))
;;   (setq prsj-functions nil)
;;   )

;; (defun prsj-set-functions (s)
;;   (prsj-reset-functions)
;;   (setq prsj-functions s)
;;   (dolist (l s) (eval l))
;;   )

;; ;; Some more variables:

;; ;; the frame that exists on startup
;; (defvar prsj-initial-frame nil)

;; ;; this is put into minor-mode-alist
;; (defvar prosjekt-mode t)

;; ;; where this file is in
;; (defvar prosjekt-directory)

;; ;; prosjekt version that created the files
;; (defvar prosjekt-version "0.4")

;; ;; Configuration UI
;; (eval-and-compile
;;   (defun prosjekt-setup-toggle () (interactive))
;;   (defun prosjekt-setup-quit () (interactive))
;;   (defun prsj-config-get-result (s))
;;   (defun prsj-config-reset ())
;;   (defun prsj-config-print ())
;;   (defun prsj-config-parse ())
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Small functions

;; (defun caddr (l) (car (cddr l)))

;; (defun prsj-del-list (l e)
;;   (let ((a (assoc (car e) l)))
;;     (if a
;;         (delq a l)
;;       l)))

;; (defun prsj-add-list (l e)
;;   (nconc (prsj-del-list l e) (list e))
;;   )

;; (defun prsj-next-file (l e)
;;   (and (setq e (assoc (car e) l))
;;        (cadr (memq e l))
;;        ))

;; (defun prsj-prev-file (l e)
;;   (prsj-next-file (reverse l) e)
;;   )

;;  ; replace a closed file, either by the previous or the next.
;; (defun prsj-otherfile (l f)
;;   (or (prsj-prev-file l f)
;;       (prsj-next-file l f)
;;       ))

;; ;; make relative path, but only up to the second level of ..
;; (defun prsj-relative-path (f)
;;   (let ((r (file-relative-name f prsj-directory)))
;;     (if (string-match "^\\.\\.[/\\]\\.\\.[/\\]\\.\\.[/\\]" r)
;;         f
;;       r
;;       )))

;; ;; friendly truncate filename
;; (defun prsj-shortname (s)
;;   (let ((l (length s)) (x 30) n)
;;     (cond ((>= x l) s)
;;           ((progn
;;              (setq x (- x 3))
;;              (setq n (length (file-name-nondirectory s)))
;;              (if (< n l) (setq n (1+ n)))
;;              (>= x n)
;;              )
;;            (concat (substring s 0 (- x n)) "..." (substring s (- n)))
;;            )
;;           ((= n l)
;;            (concat (substring s 0 x) "...")
;;            )
;;           (t
;;            (concat "..." (substring s (- n) (- (- x 3) n)) "...")
;;            ))))

;; (defun prsj-settitle ()
;;   (modify-frame-parameters
;;    nil
;;    (list (cons 'title
;;                (and prsj-current
;;                     (format "emacs - %s" (car prsj-current))
;;                     )))))

;; (defun prosjekt-addon (f)
;;   (concat prosjekt-directory f)
;;   )

;; (defun prsj-goto-line (n)
;;   (goto-char 1)
;;   (beginning-of-line n)
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Write configuration to file

;; (defun prsj-print-list (s fp)
;;   (let ((v (eval s)))
;;     (setq v (list 'setq s
;;       (if (and (atom v) (null (and (symbolp v) v)))
;;           v
;;           (list 'quote v)
;;           )))
;;     ;;(print v fp)
;;     (pp v fp) (princ "\n" fp)
;;     ))

;; (defun prsj-create-file (filename)
;;   (let ((fp (generate-new-buffer filename)))
;;     (princ ";; -*- mode: Lisp; -*-\n\n" fp)
;;     fp))

;; (defun prsj-close-file (fp)
;;   (with-current-buffer fp
;;     (condition-case nil
;;       (and t (write-region nil nil (buffer-name fp) nil 0))
;;       (error nil)
;;       ))
;;   (kill-buffer fp)
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Load/Save global project list and initial frame sizes

;; (defun prsj-loadlist ()
;;   (prsj-init)
;;   (load (prsj-globalfile) t t)
;;   (setq prsj-version prosjekt-version)
;;   )

;; (defun prsj-get-frame-pos (f)
;;   (mapcar
;;    (lambda (parm) (cons parm (frame-parameter f parm)))
;;    '(top left width height)
;;    ))

;; (defun prsj-savelist ()
;;   (let ((g (prsj-globalfile)) fp)
;;     (unless (file-exists-p g)
;;       (make-directory (file-name-directory g) t)
;;       )
;;     (setq prsj-last-open (car prsj-current))
;;     (when (frame-live-p prsj-initial-frame)
;;       (setq prsj-frame-pos (prsj-get-frame-pos prsj-initial-frame))
;;       )
;;     (setq fp (prsj-create-file g))
;;     (when fp
;;       (prsj-print-list 'prsj-version fp)
;;       (prsj-print-list 'prsj-list fp)
;;       (prsj-print-list 'prsj-last-open fp)
;;       (prsj-print-list 'prsj-frame-pos fp)
;;       (prsj-close-file fp)
;;       )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Load/Save local per-project configuration file

;; (defun prsj-update-config ()
;;   (let ((d (prsj-get-directory prsj-current))
;;         (e (prsj-getconfig "exec-root"))
;;         )
;;     (if e (setq d (expand-file-name e d)))
;;     (setq prsj-exec-directory (file-name-as-directory d))
;;     ))

;; (defun prsj-get-directory (a)
;;   (file-name-as-directory (expand-file-name (cadr a)))
;;   )

;; (defun prsj-get-cfg ()
;;   (expand-file-name (or (caddr prsj-current) prsj-default-cfg) prsj-directory)
;;   )

;; (defun prsj-get-buffer (a)
;;   (cond ((buffer-live-p (cdr a))
;;          (cdr a)
;;          )
;;         (prsj-directory
;;          (get-file-buffer (expand-file-name (car a) prsj-directory))
;;          )))

;; (defun prsj-loadconfig (a)
;;   "Load the config for a project.
;; ``a`` is a (name.dir) project cell."
;;   (let (lf e)
;;     (prsj-reset)

;;     (setq prsj-current a)
;;     (setq prsj-directory (prsj-get-directory a))

;;     ; Load the project file
;;     (when (file-regular-p (setq lf (prsj-get-cfg)))
;;       (load lf nil t)

;;       ; Update curfile to either the current file in the file list or,
;;       ; if that doesn't exist, the head of the file list.
;;       (setq prsj-curfile
;;             (or (assoc prsj-curfile prsj-files)
;;                 (car prsj-files)
;;                 ))
;;       )

    
;;     (if (setq e (prsj-getconfig "project-name")); If there's a name in
;; 						; the project config
;; 						; file...
;;         (setcar a e)	                        ; ...set it in the
;; 						; project def.
;;         (prsj-setconfig "project-name" (car a)) ; ...else, update the
;; 						; config from the
;; 						; project def.
;;         )

;;     (prsj-update-config)
;;     (prsj-set-functions prsj-functions)
;;     (setq prsj-version prosjekt-version)
;;     ))

;; (defun prsj-saveconfig ()
;;   (when prsj-current
;;     (let (w c b files)
;;       (prsj-removehooks)
;;       (setq w (selected-window))
;;       (setq c (window-buffer w))
;;       (dolist (a prsj-files)
;;         (setq b (prsj-get-buffer a))
;;         (cond (b
;;                (set-window-buffer w b t)
;;                (with-current-buffer b
;;                  (let ((s (line-number-at-pos (window-start w)))
;;                        (p (line-number-at-pos (window-point w)))
;;                        )
;;                    (push (list (car a) s p) files)
;;                    )))
;;               ((consp (cdr a))
;;                (push a files)
;;                )
;;               (t
;;                (push (list (car a)) files)
;;                )))
;;       (set-window-buffer w c t)
;;       (prsj-addhooks)
;;       (let ((fp (prsj-create-file (prsj-get-cfg)))
;;             (prsj-curfile (car prsj-curfile))
;;             (prsj-files (nreverse files))
;;             )
;;         (when fp
;;           (prsj-print-list 'prsj-version fp)
;;           (prsj-print-list 'prsj-config fp)
;;           (prsj-print-list 'prsj-tools fp)
;;           (prsj-print-list 'prsj-files fp)
;;           (prsj-print-list 'prsj-curfile fp)
;;           (prsj-print-list 'prsj-functions fp)
;;           (prsj-close-file fp)
;;           ))
;;       )))

;; (defun prsj-saveall ()
;;   (prsj-saveconfig)
;;   (prsj-savelist)
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; The core functions:  Open / Close / Add / Remove  Project

;; (defun prosjekt-open (proj)
;;   "Open another project. 
;; ``proj`` can be a project name or a cons cell (name.directory)"

;;   ;; TODO: This should only accept one form of argument. We need to
;;   ;; change this and then clean up callers who don't respect it.

;;   (interactive
;;    (list
;;     (or (prsj-config-get-result 'p)
;;         (completing-read "Open Project: " (mapcar 'car prsj-list))
;;         )))

;;   ; If a is just a project name, look up the (name.dir) cell in the
;;   ; project list.
;;   (unless (consp proj)
;;     (let ((proj_in_list (assoc proj prsj-list)))
;;       (unless proj_in_list
;;         (error "No such project: %s" proj)
;;         )
;;       (setq proj proj_in_list)
;;       ))

;;   ; Get the project def from the list (in case the argument a was not
;;   ; from that list)
;;   (setq proj 
;; 	(or 
;; 	 (car (member proj prsj-list)) ; Look up in project list 
;; 	 proj))                      

;;   ; If a is not the current project, we need to close the current
;;   ; project and open a
;;   (unless (eq a prsj-current)
;;     (unless (file-directory-p (prsj-get-directory a))
;;       (error "No such directory: %s" (cadr a))
;;       )
    
;;     ; Move a to the front of the list
;;     (setq prsj-list (cons a (delq a prsj-list)))
    
;;     ; Close the current project
;;     (prosjekt-close)

;;     ; Load the project config for a
;;     (prsj-loadconfig a)
;;     )

;;   (prsj-addhooks)
;;   (prsj-setup-all)
;;   (prsj-isearch-setup)
;;   (unless (prsj-edit-file prsj-curfile)
;;     (prosjekt-dired)
;;     ))

;; (defun prosjekt-close ()
;;   "Close the current project."
;;   (interactive)
;;   (when prsj-current
;;     (prsj-saveconfig)
;;     (prsj-removehooks)
;;     (let (f)
;;       (unwind-protect
;;           (progn
;;             (save-some-buffers nil)
;;             (prosjekt-killbuffers t)
;;             (setq f t)
;;             )
;;         (or f (prsj-addhooks))
;;         ))
;;     (prsj-reset)
;;     (prsj-config-reset)
;;     (prsj-setup-all)
;;     (prsj-isearch-setup)
;;     ))

;; (defun prosjekt-killbuffers (&optional from-project)
;;   "If called interactively kills all buffers that do not belong to project files"
;;   (interactive)
;;   (let (l b)
;;     (dolist (a prsj-files)
;;       (setq b (prsj-get-buffer a))
;;       (if b (setq l (cons (list b) l)))
;;       )
;;     (dolist (b (buffer-list))
;;       (when (eq (consp (assoc b l)) from-project)
;;         (kill-buffer b)
;;         ))))

;; (defun prosjekt-add (dir &optional name cfg)
;;   "Add a new or existing project to the list."
;;   (interactive
;;    (let (d n f)
;;     (setq d (read-directory-name "Add project in directory: " prsj-directory nil t))
;;     (setq n (file-name-nondirectory (directory-file-name d)))
;;     (setq n (read-string "Project name: " n))
;;     (setq f (read-string "Project file: " prsj-default-cfg))
;;     (list d n f)
;;     ))
;;   (when dir
;;     (setq dir (directory-file-name dir))
;;     (unless name 
;;       (setq name (file-name-nondirectory dir))
;;       )
;;     (when (and cfg (string-equal cfg prsj-default-cfg))
;;       (setq cfg nil)
;;       )
;;     (let ((a (if cfg (list name dir cfg) (list name dir))))
;;       (push a prsj-list)
;;       (prosjekt-open a)
;;       )))

;; (defun prosjekt-remove (a)
;;   "Remove a project from the list."
;;   (interactive
;;    (list
;;     (or (prsj-config-get-result 'p)
;;         (completing-read "Remove project: " (mapcar 'car prsj-list))
;;         )))
;;   (unless (consp a)
;;     (let ((b (assoc a prsj-list)))
;;       (unless b
;;         (error "No such project: %s" a)
;;         )
;;       (setq a b)
;;       ))
;;   (when (progn
;;           (beep)
;;           (prog1 (y-or-n-p (format "Remove \"%s\"? " (car a)))
;;             (message "")
;;             ))
;;     (setq prsj-list (prsj-del-list prsj-list a))
;;     (prsj-setup-all)
;;     ))

;; (defun prosjekt-save ()
;;   "Save the project configuration to file."
;;   (interactive)
;;   (prsj-config-parse)
;;   (prsj-config-print)
;;   (prsj-saveall)
;;   )

;; (defun prosjekt-revert ()
;;   "Reload the project configuration from file."
;;   (interactive)
;;   (prsj-loadlist)
;;   (if prsj-current
;;       (prsj-loadconfig prsj-current)
;;     )
;;   (prsj-setup-all)
;;   )

;; (defun prosjekt-addfile (f)
;;   "Add a file to the current project."
;;   (interactive
;;    (and prsj-current
;;         (list
;;          (read-file-name "Add file to project: " nil nil t nil)
;;          )))
;;   (unless prsj-current (error "No project open"))
;;   (prsj-insert-file f (prsj-config-get-result 'f))
;;   (prsj-config-print)
;;   (prsj-setmenu)
;;   )

;; (defun prosjekt-removefile (a)
;;   "Remove a file from the current project."
;;   (interactive (prsj-get-existing-file-1 "Remove file from project: "))
;;   (setq a (prsj-get-existing-file-2 a))
;;   (prsj-remove-file a)
;;   )

;; (defun prosjekt-visitfile (a)
;;   "Visit a file from the current project."
;;   (interactive (prsj-get-existing-file-1 "Visit file: "))
;;   (setq a (prsj-get-existing-file-2 a))
;;   (prsj-edit-file a)
;;    )

;; (defun prsj-get-existing-file-1 (msg)
;;   (and prsj-current
;;        (list
;;         (or (prsj-config-get-result 'f)
;;             (completing-read msg (mapcar 'car prsj-files))
;;             ))))

;; (defun prsj-get-existing-file-2 (a)
;;    (unless prsj-current (error "No project open"))
;;    (if (consp a)
;;        a
;;      (let ((b (assoc (prsj-relative-path a) prsj-files)))
;;        (unless b (error "No such file in project: %s" a))
;;        b
;;        )))

;; (defun prosjekt-help ()
;;   "Show the prosjekt README."
;;   (interactive)
;;   (view-file (prosjekt-addon "prosjekt.txt"))
;;   )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Hook functions to track opening/closing files from emacs

;; (defun prsj-addhooks ()
;;   (when prsj-autotracking
;;     (add-hook 'kill-buffer-hook 'prsj-kill-buffer-hook)
;;     (add-hook 'find-file-hook 'prsj-find-file-hook)
;;     (add-hook 'window-configuration-change-hook 'prsj-wcc-hook)
;;     ))

;; (defun prsj-removehooks ()
;;   (remove-hook 'window-configuration-change-hook 'prsj-wcc-hook)
;;   (remove-hook 'find-file-hook 'prsj-find-file-hook)
;;   (remove-hook 'kill-buffer-hook 'prsj-kill-buffer-hook)
;;   )

;; (defun prsj-wcc-hook ()
;;   (dolist (w (window-list))
;;     (prsj-register-buffer (window-buffer w))
;;     ))

;; (defun prsj-find-file-hook ()
;;   (run-with-idle-timer 0.2 nil 'prsj-wcc-hook)
;;   )

;; (defun prsj-kill-buffer-hook ()
;;   (let ((b (current-buffer)) a)
;;     (if (setq a (rassq b prsj-files))
;;         (prsj-remove-file a t)
;;         (if (setq a (rassq b prsj-removed-files))
;;             (setq prsj-removed-files (delq a prsj-removed-files))
;;           ))))

;; (defun prsj-register-buffer (b)
;;   (let (f a)
;;     (setq f (buffer-file-name b))
;;     (when (and f t) ;;(not (string-match "^\\." (file-name-nondirectory f))))
;;       (setq a (rassq b prsj-files))
;;       (unless a
;;         (setq a (prsj-insert-file f nil t))
;;         (when a
;;           (prsj-init-buffer a b)
;;           ))
;;       (when (and a (null (eq a prsj-curfile)))
;;         (setq prsj-curfile a)
;;         (prsj-setmenu)
;;         ))
;;     a))

;; (defun prsj-insert-file (f &optional after on-the-fly)
;;   (let ((r (prsj-relative-path f)) a m)
;;     (setq a (assoc r prsj-files))
;;     (unless (or a (and on-the-fly (assoc r prsj-removed-files)))
;;       (setq a (list r))
;;       (setq m (memq (or after prsj-curfile) prsj-files))
;;       (if m
;;           (setcdr m (cons a (cdr m)))
;;           (setq prsj-files (prsj-add-list prsj-files a))
;;           )
;;       (setq prsj-removed-files (prsj-del-list prsj-removed-files a))
;;       (message "Added to project: %s" r)
;;       )
;;     a))

;; (defun prsj-remove-file (a &optional on-the-fly)
;;   (let ((n (prsj-otherfile prsj-files a)) b)
;;     (setq prsj-files (prsj-del-list prsj-files a))
;;     (when (eq prsj-curfile a)
;;       (setq prsj-curfile n)
;;       )
;;     (unless on-the-fly
;;         (setq prsj-removed-files (prsj-add-list prsj-removed-files a))
;;         )
;;     (unless (prsj-config-print)
;;       (prsj-edit-file prsj-curfile)
;;       )
;;     (prsj-setmenu)
;;     (message "Removed from project: %s" (car a))
;;     ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Edit another file

;; (defun prsj-init-buffer (a b)
;;   (with-current-buffer b
;;     (when prsj-rename-buffers
;;       (rename-buffer (car a) t)
;;       )
;;     (when prsj-set-default-directory
;;       (cd prsj-directory)
;;       ))
;;   (setcdr a b)
;;   )

;; (defun prsj-find-file (a)
;;   (when a
;;     (let (b pos f)
;;       (setq b (prsj-get-buffer a))
;;       (unless b
;;         (prsj-removehooks)
;;         (setq f (expand-file-name (car a) prsj-directory))
;;         (setq b (find-file-noselect f))
;;         (prsj-addhooks)
;;         (when (and b (consp (cdr a)))
;;           (setq pos (cdr a))
;;           ))
;;       (when b
;;         (prsj-init-buffer a b)
;;         (cons b pos)
;;         ))))

;; (defun prsj-edit-file (a)
;;   (let ((f (prsj-find-file a)))
;;     (when f
;;       (prosjekt-setup-quit)
;;       (switch-to-buffer (car f))
;;       (prsj-restore-edit-pos (cdr f) (selected-window))
;;       (prsj-setmenu)
;;       ;;(message "dir: %s" default-directory)
;;       )
;;     (setq prsj-curfile a)
;;     ))

;; (defun prsj-restore-edit-pos (pos w)
;;   (let ((top (car pos)) (line (cadr pos)))
;;     (when (and (numberp top) (numberp line))
;;       (prsj-goto-line top)
;;       (set-window-start w (point))
;;       (prsj-goto-line line)
;;       )))

;; (defun prsj-select-window (w)
;;   (let (focus-follows-mouse)
;;     (select-window w)
;;     (select-frame-set-input-focus (window-frame w))
;;     ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; choose next/previous file

;; ;;;###autoload
;; (defun prosjekt-nextfile ()
;;   "Switch to the next file that belongs to the current project."
;;   (interactive)
;;   (prsj-switch-file 'prsj-next-file 'next-buffer)
;;   )

;; ;;;###autoload
;; (defun prosjekt-prevfile ()
;;   "Switch to the previous file that belongs to the current project."
;;   (interactive)
;;   (prsj-switch-file 'prsj-prev-file 'previous-buffer)
;;   )

;; (defun prsj-switch-file (fn1 fn2)
;;   (let ((a (rassoc (current-buffer) prsj-files)))
;;     (cond (a
;;            (prsj-edit-file (or (funcall fn1 prsj-files a) a))
;;            )
;;           (prsj-curfile
;;            (prsj-edit-file prsj-curfile)
;;            )
;;           (t
;;            (funcall fn2)
;;            ))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Set key shortcuts

;; (defun prsj-setkeys ()
;;   (let ((f (consp prsj-current))
;;         (a (assoc 'prosjekt-mode minor-mode-map-alist))
;;         (map (make-sparse-keymap))
;;         )
;;     (if a
;;         (setcdr a map)
;;         (push (cons 'prosjekt-mode map) minor-mode-map-alist)
;;         )
;;     (dolist (k prsj-keybindings)
;;       (when (or f (eq (caddr k) 'always))
;;         (define-key map (car k) (cadr k))
;;         ))

;;     (when f
;;       (let ((n 0) fn s)
;;         (dolist (a prsj-tools)
;;           (unless (setq fn (nth n prsj-tools-fns))
;;             (setq fn (list 'lambda))
;;             (setq prsj-tools-fns (nconc prsj-tools-fns (list fn)))
;;             )
;;           (setcdr fn `(() (interactive) (prsj-run-tool ',a)))
;;           (setq n (1+ n))
;;           (when (setq s (caddr a))
;;             (define-key map (prsj-parse-key s) (and f fn))
;;             ))))))

;; (defun prsj-parse-key (s)
;;   (read
;;    (if (string-match "[a-z][a-z0-9]+$" s)
;;        (concat "[" s "]")
;;        (concat "\"\\" s "\""))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Set menus

;; (defun prsj-list-sorted ()
;;   (sort (append prsj-list nil)
;;         '(lambda (a b) (string-lessp (car a) (car b)))
;;         ))

;; (defun prsj-setmenu ()
;;   (let ((f (consp prsj-current)) m1 m2 m3)

;;     (setq m1
;;           `(("Open" open ,@(prsj-menulist-maker prsj-list prsj-current 'prsj-menu-open))
;;             ("Add/Remove" other
;;              ("Add ..." "Add new or existing project to the list" . prosjekt-add)
;;              ("Remove ..." "Remove project from the list" . prosjekt-remove)
;;              ,@(and f '(("Close" "Close current project" . prosjekt-close)))
;;              ("--")
;;              ("Setup" "Enter the project setup area." . prosjekt-setup-toggle)
;;              ("Help" "View prosjekt.txt" . prosjekt-help)
;;              )
;;             ))
;;     (when f
;;       (nconc m1 (cons '("--") (prsj-menulist-maker prsj-tools nil prsj-tools-fns)))
;;       (setq m2
;;             `(("Dired" "Browse project directory in Dired - Use 'a' to add file(s) to the project" . prosjekt-dired)
;;               ("--")
;;               ,@(prsj-menulist-maker prsj-files prsj-curfile 'prsj-menu-edit)
;;               )))

;;     (prsj-menu-maker
;;      global-map
;;      `((buffer "Project" project ,@m1)
;;        (file "List" list ,@m2)
;;        )
;;      '(menu-bar)
;;      )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun prsj-menu-edit ()
;;   (interactive)
;;   (let ((a (nth last-command-event prsj-files)))
;;     (if a (prsj-edit-file a))
;;     ))

;; (defun prsj-menu-open ()
;;   (interactive)
;;   (let ((a (nth last-command-event prsj-list)))
;;     (if a (prosjekt-open (car a)))
;;     ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun prsj-menu-maker (map l v)
;;   (let ((e (list nil)))
;;     (setq v (append v e))
;;     (dolist (k (reverse l))
;;       (let (s a)
;;         (when (symbolp (car k))
;;           (setq a (pop k))
;;           )
;;         (cond
;;          ((numberp (car k))
;;           (setcar e (pop k))
;;           )
;;          ((and (consp (cdr k)) (symbolp (cadr k)))
;;           (setcar e (cadr k))
;;           (setq s (cddr k))
;;           (setq k (and s (cons (car k) (make-sparse-keymap (car k)))))
;;           )
;;          (t
;;           (setcar e (intern (downcase (car k))))
;;           ))
;;         (if a
;;             (define-key-after map (vconcat v) k a)
;;             (define-key map (vconcat v) k)
;;             )
;;         (if s (prsj-menu-maker map s v))
;;         ))))

;; (defun prsj-copy-head (l n)
;;   (let (r)
;;     (while (and l (> n 0))
;;       (push (pop l) r)
;;       (setq n (1- n))
;;       )
;;     (nreverse r)
;;     ))

;; (defun prsj-split-list (l n)
;;   (let (r)
;;     (while l
;;       (push (prsj-copy-head l n) r)
;;       (setq l (nthcdr n l))
;;       )
;;     (nreverse r)
;;     ))

;; (defun prsj-menulist-maker (l act fns)
;;   (let (r (w 30) s (m 0) (n 0) k)
;;     (cond
;;      ((< (length l) w)
;;       (prsj-menulist-maker-1 (list l fns n) act)
;;       )
;;      (t
;;       ;; menu too long; split into submenus
;;       (setq s (prsj-split-list l w))
;;       (setq k (prsj-menulist-maker-1 (list (append (pop s) '(("--"))) fns n) act))
;;       (setq r (nreverse k))
;;       (dolist (l s)
;;         (when (consp fns)
;;           (setq fns (nthcdr w fns))
;;           )
;;         (setq n (+ n w))
;;         (setq k (prsj-menulist-maker-1 (list l fns n) act))
;;         (push (cons (concat (prsj-shortname (caar l)) " ...")
;;                     (cons (intern (format "m_%d" (setq m (1+ m))))
;;                           k)) r)
;;         )
;;       (nreverse r)
;;       ))))

;; (defun prsj-menulist-maker-1 (l act)
;;   (let (r e f s i n a)
;;     (while (car l)
;;       (setq a (caar l))
;;       (setcar l (cdar l))
;;       (setq n (caddr l))
;;       (setcar (cddr l) (1+ n))
;;       (setq f (if (consp (cadr l))
;;                   (prog1 (car (cadr l)) (setcar (cdr l) (cdr (cadr l))))
;;                   (cadr l)))

;;       (setq i (car a))
;;       (unless (string-match "^ *#" i)
;;         (setq s (if (and (consp (cdr a)) (stringp (cadr a))) (cadr a) i))
;;         (cond ((equal ">" i)
;;                (setq e (cons s (cons (intern s) (prsj-menulist-maker-1 l act))))
;;                (setq r (cons e r))
;;                )
;;               ((equal "<" i)
;;                (setq l nil)
;;                )
;;               (t
;;                (setq i (prsj-shortname i))
;;                (setq e (cons n (if (eq a act)
;;                                    `(menu-item ,i ,f :button (:toggle . t) :help ,s)
;;                                  (cons i (cons s f)))))
;;                (setq r (cons e r))
;;                )))
;;       )
;;     (nreverse r)
;;     ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Run make and other commands

;; (defun prsj-compilation-in-frame (cmd)
;;   (let ((bn "*compilation*") w h b c f)
;;     (unless (get-buffer-window bn t)
;;       (setq b (get-buffer-create bn))
;;       (setq f (frame-list))
;;       (cond ((cdr f)
;;              (setq w (frame-first-window (car f)))
;;              (delete-other-windows w)
;;              )
;;             (t
;;              (setq h (/ (* 70 (frame-height)) 100))
;;              (delete-other-windows w)
;;              (setq w (split-window w h))
;;              ))
;;       (set-window-buffer w b)
;;       ))
;;   (let ((display-buffer-reuse-frames t) (f (selected-frame)))
;;     (compile cmd)
;;     (select-frame-set-input-focus f)
;;     ))

;; (defun prsj-run (cmd)
;;   (cond ((string-match "^-e +" cmd)
;;          (setq cmd (read (substring cmd (match-end 0))))
;;          (unless (commandp cmd)
;;            (setq cmd `(lambda () (interactive) ,cmd))
;;            )
;;          (command-execute cmd)
;;          )
;;         ((let ((b (current-buffer)) 
;;                (old-dir default-directory) 
;;                (new-dir ".")
;;                )
;;            (when (string-match "^-in +\\([^[:space:]]+\\) +" cmd)
;;              (setq new-dir (match-string-no-properties 1 cmd))
;;              (setq cmd (substring cmd (match-end 0)))
;;              )
;;            (when prsj-exec-directory
;;              (setq new-dir (expand-file-name new-dir prsj-exec-directory))
;;              )
;;            (cd new-dir)
;;            (cond ((string-match "\\(.+\\)& *$" cmd)
;;                   (start-process-shell-command 
;;                    "prosjekt-async" nil (match-string 1 cmd))
;;                   (message (match-string 1 cmd))
;;                   )
;;                  (prsj-set-compilation-frame
;;                   (prsj-compilation-in-frame cmd)
;;                   )
;;                  (t
;;                   (compile cmd)
;;                   ))
;;            (with-current-buffer b (cd old-dir))
;;            ))))
        
;; (defun prsj-run-tool (a)
;;   (unless (string-match "^--+$" (car a))
;;     (prsj-run (or (cadr a) (car a)))
;;     ))

;; (defun prosjekt-killtool ()
;;   (interactive)
;;   (let ((bn "*compilation*") w0 w1)
;;     (when (setq w1 (get-buffer-window bn t))
;;       (when (fboundp 'kill-compilation)
;;         (setq w0 (selected-window))
;;         (select-window w1)
;;         (kill-compilation)
;;         (select-window w0)
;;         ))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; run grep on project files

;; (defun prosjekt-grep (command-args)
;;   "Run the grep command on all the project files."
;;   (interactive
;;    (progn
;;      (require 'grep)
;;      (grep-compute-defaults)
;;      (let ((default (grep-default-command)))
;;        (list (read-from-minibuffer
;;               "Run grep on project files: "
;;               (if current-prefix-arg default grep-command)
;;               nil
;;               nil
;;               'grep-history
;;               (if current-prefix-arg nil default)
;;               )))))
;;   (let ((b (current-buffer)) (old-dir default-directory))
;;     (dolist (f (mapcar 'car prsj-files))
;;       (setq command-args (concat command-args " " f))
;;       )
;;     (when prsj-directory (cd prsj-directory))
;;     (grep command-args)
;;     (with-current-buffer b (cd old-dir))
;;     ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; add files to the project with dired

;; (require 'dired)

;; ;;;###autoload
;; (defun prsj-dired-addfiles ()
;;   (interactive)
;;   (when prsj-current
;;     (let ((n 0) a)
;;       (dolist (f (dired-get-marked-files))
;;         (setq a (prsj-insert-file f))
;;         (unless (cdr a)
;;           (setq n (1+ n))
;;           (setq prsj-curfile a)
;;           ))
;;       (if (> n 1) (message "Added to project: %d file(s)" n))
;;       (prsj-setmenu)
;;       )))

;; ;;;###autoload
;; (defun prosjekt-dired ()
;;   "Start a dired window with the project directory."
;;   (interactive)
;;   (when prsj-directory
;;     (prosjekt-setup-quit)
;;     ;;(message "Use 'a' to add marked or single files to the project.")
;;     (dired prsj-directory)
;;     (let ((map dired-mode-map))
;;       (define-key map "a" 'prsj-dired-addfiles)
;;       (define-key map [menu-bar operate command] '("Add to Project"
;;         "Add current or marked file(s) to project" . prsj-dired-addfiles))
;;       )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun prsj-setup-all ()
;;   (prsj-setkeys)
;;   (prsj-setmenu)
;;   (prsj-settitle)
;;   (prsj-config-print)
;; )

;; (defun prsj-getconfig (n)
;;   (cdr (assoc n prsj-config)))

;; (defun prsj-setconfig (n v)
;;   (let ((a (assoc n prsj-config)))
;;     (unless a
;;       (setq a (list n))
;;       (setq prsj-config (nconc prsj-config (list a)))
;;       )
;;     (setcdr a v)
;;     ))

;; (defun prsj-on-kill ()
;;   (prsj-saveall)
;; )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; isearch in all project files

;; (defun prsj-isearch-function (b wrap)
;;   (let (a)
;;     (or b (setq b (current-buffer)))
;;     (cond (wrap
;;            (if isearch-forward
;;                (setq a (car prsj-files))
;;                (setq a (car (last prsj-files)))
;;                ))
;;           ((setq a (rassoc b prsj-files))
;;            (if isearch-forward
;;                (setq a (prsj-next-file prsj-files a))
;;                (setq a (prsj-prev-file prsj-files a))
;;                )
;;             ))
;;     (car (prsj-find-file a))
;;     ;; (print `(prsj-isearch (wrap . ,wrap) ,b ,d) (get-buffer "*Messages*"))
;;     ))

;; (defun prsj-isearch-setup ()
;;   (cond ((and prsj-set-multi-isearch prsj-current)
;;          (setq multi-isearch-next-buffer-function 'prsj-isearch-function)
;;          (setq multi-isearch-pause 'initial)
;;          (add-hook 'isearch-mode-hook 'multi-isearch-setup)
;;          )
;;         (t
;;          (remove-hook 'isearch-mode-hook 'multi-isearch-setup)
;;          )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Initialize

;; (defun prsj-startup-delayed ()
;;   ;; load UI support
;;   (load (prosjekt-addon "prosjekt-config") nil t)

;;   ;; When no projects are specified yet, load the prosjekt project itself.
;;   (unless prsj-list
;;     (load (prosjekt-addon prsj-default-cfg))
;;     )

;;   ;; no project so far
;;   (prsj-reset)
;;   (prsj-setup-all)
;;   (add-hook 'kill-emacs-hook 'prsj-on-kill)

;;   ;; inhibit open last project when a file was on the commandline
;;   (unless (buffer-file-name (window-buffer))
;;     (when prsj-last-open

;;       ;; open last project
;;       (prosjekt-open prsj-last-open)

;;       ;; emacs bug: deferred jit-lock is dropped if run from idle timer
;;       (and jit-lock-mode jit-lock-defer-time (jit-lock-function (point)))

;;       ;; restore frame position
;;       (when (and prsj-set-framepos prsj-frame-pos prsj-initial-frame)
;;         (modify-frame-parameters prsj-initial-frame prsj-frame-pos)
;;         ;; emacs bug: when it's too busy it doesn't set frames correctly.
;;         (sit-for 0.2)
;;         ))))

;; (defun prsj-command-line-switch (option)
;;   (setq prsj-last-open (pop argv))
;;   (setq inhibit-startup-screen t)
;;   )

;; (defun prosjekt-startup ()
;;   ;; where is this file
;;   (if load-file-name
;;       (setq prosjekt-directory (file-name-directory load-file-name)))
;;   (if (boundp 'prsj-list)
;;     (progn
;;       (load (prosjekt-addon "prosjekt-config"))
;;       (prsj-setup-all))
;;     (progn
;;       (prsj-loadlist)
;;       (when prsj-last-open (setq inhibit-startup-screen t))
;;       (when (display-graphic-p) (setq prsj-initial-frame (selected-frame)))
;;       (push '("project" . prsj-command-line-switch) command-switch-alist)
;;       (run-with-idle-timer 0.1 nil 'prsj-startup-delayed)
;;       )))


;; ;;; prosjekt-populate

;; ; TODO: Add support for skipping common "untracked" directories,
;; ; e.g. .hg, .svn, etc.

;; ; TODO: Some sort of auto-populate? Picks up everything that matches
;; ; common source-file extensions

;; (defun prsj-walk-path (dir action)
;;   "walk DIR executing ACTION with (dir file)"
;;   (cond ((file-directory-p dir)
;; 	 (or (char-equal ?/ (aref dir(1- (length dir))))
;; 	     (setq dir (file-name-as-directory dir)))
;; 	 (let ((lst (directory-files dir nil nil t))
;; 	       fullname file)
;; 	   (while lst
;; 	     (setq file (car lst))
;; 	     (setq lst (cdr lst))
;; 	     (cond ((member file '("." "..")))
;; 		   (t
;; 		    (and (funcall action dir file)
;; 			 (setq fullname (concat dir file))
;; 			 (file-directory-p fullname)
;; 			 (prsj-walk-path fullname action)))))))
;; 	(t
;; 	 (funcall action
;; 		  (file-name-directory dir)
;; 		  (file-name-nondirectory dir)))))

;; (defun prsj-add-if (p dir file)
;;   "If `file` matches the regex `p`, dir+file is added to the project."
;;   (if (string-match p file)
;;       (prsj-insert-file (concat dir file) (prsj-config-get-result 'f))
;;       't))

;; (defun prosjekt-populate (dir p)
;;   "Add all files under DIR which match regex P to the project."
;;   (interactive
;;    (list 
;;     (read-directory-name "Directory: " prsj-directory)
;;     (read-string "Pattern: " "")))
;;   (unless prsj-current (error "No project open"))

;;   ; TODO: Verify that `dir` is under prsj-directory? Is this required?

;;   (when p
;;     (prsj-walk-path dir
;; 	       (lambda (dir file) (prsj-add-if p dir file)))))

;; (defun prosjekt-repopulate ()
;;   "Repopulate the project based on project-populate-spec."
;;   (interactive)
;;   (unless (prsj-getconfig "populate-spec") (error "No populate-spec defined."))
;;   (unless prsj-directory (error "No prsj-directory defined."))
;;   (prosjekt-clear)
;;   (let ((spec (prsj-getconfig "populate-spec")))
;;     (while spec
;;                                         ; path is the current
;;                                         ; project-relative
;;                                         ; subdirectory
;;       (setq path (caar spec))

;;                                         ; patterns is the list of
;;                                         ; patterns to populate with
;;                                         ; from that path
;;       (setq patterns (cdar spec))
;;       (setq spec (cdr spec))
;;       (while patterns
;;         (setq pattern (car patterns))
;;         (setq patterns (cdr patterns))

;;                                         ; populate using the specified
;;                                         ; path and pattern
;;         (prosjekt-populate (concat prsj-directory path) pattern)))))

;; (defun prosjekt-clear ()
;;   (interactive)
;;   (unless prsj-current (error "No project open"))
;;   (setq prsj-files nil))

;;;###autoload(require 'prosjekt)
(provide 'prosjekt)
(prosjekt-startup)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; prosjekt.el ends here
