;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; prosjekt-config.el --- project workspaces for emacs --- UI part
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer
(defvar prsj-buffer nil)
;; keymap
(defvar prsj-browse-map nil)
;; overlays
(defvar prsj-hilight-bar nil)
(defvar prsj-hilight-bar-2 nil)
;; flag
(defvar prsj-edit-mode nil)

;; tabs
(defvar prsj-groups)
(defvar prsj-active-group nil)
(defvar prsj-group-top nil)
(defvar prsj-group-left nil)
(defvar prsj-group-tab nil)

;; tab menus
(defvar prsj-links)

;; quick search
(defvar prsj-qs-face nil)
(defvar prsj-qs-str nil)
(defvar prsj-qs-len nil)
(defvar prsj-qs-pos nil)

;; from prosjekt.el
(defvar prsj-list)
(defvar prsj-current)
(defvar prsj-files)
(defvar prsj-curfile)
(defvar prsj-config)
(defvar prsj-tools)
;; also
(declare-function prsj-setconfig "prosjekt")
(declare-function prsj-getconfig "prosjekt")
(declare-function prsj-setup-all "prosjekt")
(declare-function prsj-remove-file "prosjekt")
(declare-function caddr "prosjekt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get the (cdr e) property from plist (car e)
(defmacro p-get (e)
  `(plist-get ,(car e) ',(cdr e))
  )
(defmacro p-set (e v)
  `(plist-put ,(car e) ',(cdr e) ,v)
  )
(defmacro p-call (e &rest args)
  `(funcall (plist-get ,(car e) ',(cdr e)) ,@args)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show/Hide the *prosjekt* buffer

;;;###autoload
(defun prosjekt-setup ()
  "Show the configuration buffer."
  (interactive)
  (let ((map (make-keymap)))

    (substitute-key-definition
      'self-insert-command
      'prsj-qsearch
      map
      global-map
      )

    (dolist (k '(
        ("\t" . prsj-next-button)
        ([tab] . prsj-next-button)
        ("\e\t" . prsj-prev-button)
        ([S-tab] . prsj-prev-button)
        ([backtab] . prsj-prev-button)

        ([left] . prsj-move-left)
        ([right] . prsj-move-right)
        ([backspace] . prsj-qsearch)
        ([delete] . prsj-qsearch)
        ([127] . prsj-qsearch)
        ([return] . prsj-enter)

        ([32] . prosjekt-edit)
        ([escape] . prosjekt-setup-quit)

        ([down-mouse-1] . prsj-mouse)
        ([down-mouse-2] . prsj-mouse)
        ([mouse-1] . prsj-mouse)
        ([mouse-2] . prsj-mouse)
        ([mouse-3] . ignore)
        ([drag-mouse-1] . ignore)
        ))
      (define-key map (car k) (cdr k))
      )

    (cond ((buffer-live-p prsj-buffer)
           (switch-to-buffer prsj-buffer)
           )
          (t
           (unless prsj-buffer
             (add-hook 'post-command-hook 'prsj-post-command-hook)
             )
           (prsj-config-init)
           (setq prsj-buffer (get-buffer-create "*prosjekt*"))
           (switch-to-buffer prsj-buffer)
           ))

    (setq prsj-browse-map map)
    (prsj-qs-clear)
    (unless prsj-edit-mode
      (use-local-map map)
      (prsj-config-print)
      )
    ))

(defun prosjekt-setup-quit ()
  "Kill the configuration buffer."
  (interactive)
  (let ((alive (buffer-live-p prsj-buffer)))
    (cond ((and alive prsj-edit-mode)
           (bury-buffer prsj-buffer)
           )
          (t
           (when alive
             (kill-buffer prsj-buffer)
             )
           (remove-hook 'post-command-hook 'prsj-post-command-hook)
           (setq prsj-buffer nil)
           ))))

(defun prosjekt-setup-toggle ()
  "Show/hide the project configuration browser."
  (interactive)
  (if (prsj-config-active)
      (prosjekt-setup-quit)
    (prosjekt-setup)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit mode

(defun prosjekt-edit ()
  (interactive)
  (if (eq 'u (car prsj-active-group)) (emacs-lisp-mode))
  (let ((map (make-sparse-keymap)))
    (define-key map [escape] 'prosjekt-edit-quit)
    (setq prsj-edit-mode t)
    (prsj-qs-clear)
    (use-local-map map)
    (prsj-config-print)
    ))

(defun prosjekt-edit-quit ()
  (interactive)
  (if (eq 'u (car prsj-active-group)) (fundamental-mode))
  (prsj-config-parse)
  (use-local-map prsj-browse-map)
  (setq prsj-edit-mode nil)
  (setq cursor-type nil)
  (prsj-set-hilite-bar)
  (prsj-setup-all)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prsj-config-active ()
  (eq (current-buffer) prsj-buffer)
  )

(defun prsj-save-window-pos ()
  (p-set (prsj-active-group . :pos)
     (list
      (window-start)
      (- (line-number-at-pos) prsj-group-top)
      )))

(defun prsj-config-reset ()
  (dolist (s prsj-groups)
    (p-set (s . :pos) (list 1 0))
    )
  (setq prsj-active-group (car prsj-groups))
  )

(defun prsj-config-init ()
  (dolist (v '(
      prsj-buffer
      prsj-browse-map
      prsj-hilight-bar
      prsj-hilight-bar-2
      prsj-edit-mode
      ))
    (set v nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read back the configuration after edits

(defun prsj-config-parse ()
  (when (and (prsj-config-active) prsj-edit-mode)
    (with-current-buffer prsj-buffer
      (save-excursion
        (let ((s (p-get (prsj-active-group . :scan))) l r e)
          (prsj-goto-line prsj-group-top)
          (if (eq 'u (car prsj-active-group))
              (setq l (read (concat
                             "(("
                             (buffer-substring-no-properties (point) (point-max))
                             "))")))
              (progn
                (while (< (point) (point-max))
                  (setq e (line-end-position))
                  (setq r
                    (if (and s (posix-search-forward (car s) e t))
                        (apply (cdr s) nil)
                        (and (re-search-forward "^ *\\(.*[^ :]\\)[ :]*$" e t)
                             (list (match-string-no-properties 1))
                             )))
                  (if r (setq l (cons r l)))
                  (forward-line 1)
                  )
                (setq l (nreverse l))
                ))
          (p-call (prsj-active-group . :parse) l)
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The project config window

;; (makunbound 'prsj-groups) (makunbound 'prsj-links)

(defvar prsj-groups `(

   (p nil
      :title "Projects"
      :comment "All projects on a list"
      :pos (1 0)
      :list prsj-list
      :exec prosjekt-open
      :print ,(lambda (a p)
               (prsj-link (car a) nil a)
               (prsj-link-2 nil p (cadr a))
               (and (caddr a) (prsj-link-2 nil p (caddr a)))
               )
      :scan ("^ *\\([^ :]+\\) *: *\\([^ ]+\\) *\\( +: *\\([^ ]+\\)\\)? *$" .
              ,(lambda ()
               (let ((a (match-string-no-properties 1))
                     (b (match-string-no-properties 2))
                     (c (match-string-no-properties 4))
                     )
                (cons a (cons b (and c (list c))))
                )))
      :parse ,(lambda (s)
               (dolist (a s)
                 (unless (cadr a)
                   (error "Error: Project directory empty: %s" (car a))
                   )
                 (when prsj-current
                   (when (string-equal (cadr a) (cadr prsj-current))
                     (setq prsj-current a)
                     (prsj-setconfig "project-name" (car a))
                     )))
               (setq prsj-list s)
               )
      :menu (add remove open close)
      )

   (f nil
      :title "Files"
      :comment "The files that belong to the project"
      :pos (1 0)
      :list prsj-files
      :exec prosjekt-visitfile
      :print ,(lambda (a p)
               (prsj-link (car a) nil a)
               )
      :scan nil
      :parse ,(lambda (s)
                (let (b)
                  (dolist (l s)
                    (setcdr l (cdr (assoc (car l) prsj-files)))
                    )
                  (dolist (a prsj-files)
                    (if (setq b (assoc (car a) s))
                        (if (eq a prsj-curfile) (setq prsj-curfile b))
                        (prsj-remove-file a)
                        ))
                  (setq prsj-files s)
                  ))
      :menu (add-file remove-file visit-file)
      )

   (t nil
      :title "Tools"
      :comment "Configurable tools and keyboard shortcuts"
      :pos (1 0)
      :list prsj-tools
      :exec prsj-run-tool
      :print ,(lambda (a p)
               (prsj-link (car a) nil a)
               (when (caddr a)
                 (unless prsj-edit-mode
                   (insert-char 32 (- (- prsj-group-tab 12) (- (point) p)))
                   )
                 (insert " (" (caddr a) ")")
                 )
               (prsj-link-2 nil p (cadr a))
               )
      :scan ("^ *\\([^(:]*[^(: ]\\) *\\(([^ ):]+)\\)?\\( *: *\\(.*[^ ]\\)?\\)? *$" .
              ,(lambda ()
                 (let ((a (match-string-no-properties 1))
                       (b (match-string-no-properties 2))
                       (c (match-string-no-properties 4))
                       )
                   (list a c (and b (substring b 1 -1)))
                   )))
      :parse ,(lambda (s)
               (setq prsj-tools s)
               )
      :menu ()
      )

   (s nil
      :title "Settings"
      :comment "Project options"
      :pos (1 0)
      :list prsj-config
      :exec prosjekt-edit
      :print ,(lambda (a p)
               (prsj-link-2 (car a) p (cdr a))
               )
      :scan ("^ *\\([^ :]+\\) *: *\\(.*[^ ]\\)? *$" .
              ,(lambda ()
                 (list (match-string-no-properties 1)
                       (match-string-no-properties 2)
                       )))
      :parse ,(lambda (s)
               (dolist (l s) (setcdr l (cadr l)))
               (let ((prsj-config s) n)
                 (setq n (prsj-getconfig "project-name"))
                 (unless (> (length n) 0)
                   (error "Error: Project name empty.")
                   )
                 (when prsj-current
                   (setcar prsj-current n)
                   ))
               (setq prsj-config s)
               (prsj-update-config)
               )
      :menu ()
      )

;;;    (u nil
;;;       :title "Functions"
;;;       :comment "ELisP Utitlities"
;;;       :pos (1 0)
;;;       :list prsj-functions
;;;       :exec prosjekt-edit
;;;       :print ,(lambda (a p)
;;;                (pp a (current-buffer))
;;;                )
;;;       :parse ,(lambda (s)
;;;                 (prsj-set-functions s)
;;;                )
;;;       :menu ()
;;;       )
   ))


(defvar prsj-links '(

   ;; projects
   (add "Add" "Add new or existing project to the list"
         prosjekt-add
         )
   (remove "Remove" "Remove a project from the the list"
         prosjekt-remove
         )
   (open "Open" "Open a Project"
         prosjekt-open
         )
   (close "Close" "Close the current project"
         prosjekt-close
         )

   ;; files
   (add-file "Add" "Add a file to the project"
         prosjekt-addfile
         )
   (remove-file "Remove" "Remove file from project"
         prosjekt-removefile
         )
   (dired "Dired" "Browse project directory - Use 'a' in dired to add file(s) to the project"
         prosjekt-dired
         )
   (visit-file "Visit" "Visit this file"
         prosjekt-visitfile
         )

   ;; edit mode
   (edit "Edit" "Edit this list (spacebar)"
         prosjekt-edit
         )
   (quit-edit "Quit" "Quit edit mode (escape)"
         prosjekt-edit-quit
         )
   (revert "Revert" "Revert all configuration to last saved state"
         prosjekt-revert
         )
   (save "Save" "Save the configuration now"
         prosjekt-save
         )

   ;; other
   (help "Help" "View the 'prosjekt' documentation."
         prosjekt-help
         )
   (quit "Quit" "Quit configuration area"
      prosjekt-setup-quit
      )
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print the config

(defun prsj-config-print ()
  (when (prsj-config-active)
    (let (x f a n title l p (inhibit-read-only t) active)

      (setq buffer-read-only nil)
      (buffer-disable-undo)
      (erase-buffer)

      (setq prsj-group-left (if prsj-edit-mode 0 1))
      (setq prsj-group-tab (+ 26 prsj-group-left))
      (setq active
            (or prsj-active-group
                (setq prsj-active-group (car prsj-groups))
                ))
      (insert "\n")
      (setq n 1)
      (dolist (s prsj-groups)
        (setq f (eq s active))
        (when (or f (and prsj-current (null prsj-edit-mode)))
          (setq title (p-get (s . :title)))
          (insert-char 32 n)
          (cond (f
                 (setq p (point))
                 (insert title)
                 (prsj-make-hilite-bar 'prsj-hilight-bar-2 p (point))
                 )
                (t
                 (prsj-link title (p-get (s . :comment)) s t)
                 ))
          (setq n 2)
          ))

      (dolist (s prsj-links)
        (prsj-define-shortcut nil (cadr s) 'ignore)
        )
      (dolist (s prsj-groups)
        (prsj-define-shortcut nil (symbol-name (car s)) 'prsj-key-set-group)
        )
      (insert "  -")
      (dolist (id (if prsj-edit-mode '(revert save quit-edit) '(edit help quit)))
         (insert "  ")
         (prsj-link-3 id nil)
         )
      (insert "\n\n  -")
      (when prsj-current
        (insert " " (car prsj-current) " ")
        )
      (insert "-")
      (unless prsj-edit-mode
        (dolist (id (p-get (active . :menu)))
          (insert "  ")
          (prsj-link-3 id nil)
          )
        )
      (insert "\n\n")

      (when prsj-edit-mode
        (add-text-properties (point-min) (point)
           '(read-only t intangible t front-sticky t rear-nonsticky t))
        )

      (setq prsj-group-top (line-number-at-pos))

      (prsj-print-items
        (p-get (active . :print))
        (eval (p-get (active . :list)))
        prsj-group-left
       )

      (setq p (p-get (active . :pos)))
      (set-window-start (get-buffer-window prsj-buffer) (car p))
      (prsj-goto-line (+ prsj-group-top (cadr p)))
      (unless (eobp)
        (forward-char prsj-group-left)
        )
      (unless (pos-visible-in-window-p)
        (recenter (/ (window-height) 5))
        )
      (set-buffer-modified-p nil)
      (cond (prsj-edit-mode
             (buffer-enable-undo)
             (setq cursor-type 'box)
             )
            (t
             (prsj-set-hilite-bar)
             (setq buffer-read-only t)
             (setq cursor-type nil)
             ))
      t
      )))

(defun prsj-print-items (fn items tab)
  (dolist (a items)
    (when (stringp (car a))
      (unless (and (string-match "^ *#" (car a)) (null prsj-edit-mode))
        (insert-char 32 tab)
        ))
    (funcall fn a (- (point) tab))
    (insert "\n")
    ))

(defun prsj-link (text help &optional fn top)
  (if (and prsj-edit-mode (null help))
      (insert text)
      (let ((p (point)) (f (if top 'link)))
        (insert-text-button
         text
         'help-echo help
         'action 'prsj-action
         'class (or fn 'link)
         'follow-link t
         'face f
         'mouse-face 'link
         )
        (when (or f help)
          (add-text-properties p (1+ p) '(face (:foreground "blue" :underline t)))
          )
        )))

(defun prsj-link-2 (a p b)
  (if a (insert a))
  (insert-char 32 (- prsj-group-tab 1 (- (point) p)))
  (if b (insert " : " b) (insert " :"))
  )

(defun prsj-link-3 (id f)
  (let ((a (assq id prsj-links)))
    (when a
      (prsj-link (cadr a) (caddr a) a f)
      (prsj-define-shortcut nil (cadr a) (nth 3 a))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project selection and configuration

(defun prsj-action (b)
  (let ((a (button-get b 'class)))
    (cond ((memq a prsj-links)
           (command-execute (nth 3 a))
           )
          ((memq a prsj-groups)
           (setq prsj-active-group a)
           (prsj-config-print)
           )
          (t
           (p-call (prsj-active-group . :exec) a)
           ))))

(defun prsj-key-set-group ()
  (interactive)
  (let ((c (intern (char-to-string (logand last-input-event 255)))) s)
      (when (setq s (assoc c prsj-groups))
        (setq prsj-active-group s)
        (prsj-config-print)
        )))

(defun prsj-define-shortcut (map s fn)
  (let ((c (logior (aref s 0) 32)))
    (define-key
      (or map (current-local-map))
      (read (format "\"\\M-%c\"" c))
      fn
      )))

(defun prsj-config-get-result (id)
  (and (prsj-config-active)
       (eq id (car prsj-active-group))
       (nth (cadr (p-get (prsj-active-group . :pos)))
            (eval (p-get (prsj-active-group . :list)))
            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab between buttons and move files up/down

(defun prsj-next-button ()
  (interactive)
  (if prsj-qs-pos
      (prsj-qs-next 1)
    ))

(defun prsj-prev-button ()
  (interactive)
  (if prsj-qs-pos
      (prsj-qs-next -1)
  ))

(defun prsj-move-left ()
  (interactive)
  (prsj-move-to -1)
  )

(defun prsj-move-right ()
  (interactive)
  (prsj-move-to 1)
  )

(defun prsj-move-to (d &optional cycle)
  (let ((n 0) (x 0))
    (dolist (s prsj-groups)
      (if (eq s prsj-active-group)
          (setq x n))
      (setq n (1+ n))
      )
    (setq x (+ x d))
    (unless prsj-current (setq n 1))
    (if cycle
        (if (< x 0) (setq x (1- n)) (if (>= x n) (setq x 0)))
        (setq x (max 0 (min (1- n) x)))
        )
    (setq prsj-active-group (nth x prsj-groups))
    (prsj-config-print)
    ))

(defun prsj-enter ()
  (interactive)
  (let (a b)
    (and (setq b (button-at (point)))
         (setq a (button-get b 'action))
         (funcall a b)
         )))

(defun prsj-mouse ()
  (interactive)
  ;;(message "LC: %s" (prin1-to-string last-input-event))
  (let ((i last-input-event) p b a x y tp)
    (when (consp i)
      (select-window (car (cadr i)))
      (setq p (nth 5 (cadr i)))
      (setq tp (nth 6 (cadr i)))
      (setq y (+ (cdr tp) (line-number-at-pos (window-start))))
      (setq x (+ (car tp) 1))
      (if (>= y prsj-group-top)
          (prsj-goto-line y)
        )
      (and (memq (car i) '(mouse-1 mouse-2))
           (setq b (button-at p))
           (setq a (button-get b 'action))
           (funcall a b)
           ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hook to maintain the selection bar

(defun prsj-post-command-hook ()
  (and
   (prsj-config-active)
   (prsj-set-hilite-bar)
   ))

(defun prsj-set-hilite-bar ()
  (unless prsj-edit-mode
    ;;(message "LC: %s" (prin1-to-string (cons this-command last-input-event)))
    (let (n m a c e p)
      (setq m (length (eval (p-get (prsj-active-group . :list)))))
      (setq p (line-number-at-pos))
      (setq n (max prsj-group-top
                   (min (line-number-at-pos)
                        (1- (+ prsj-group-top m))
                        )))
      (prsj-goto-line n)
      (if (< p n)
          (set-window-start nil (point-min))
        )
      (unless (eobp)
        (setq a (point))
        (forward-char prsj-group-left)
        (setq e (line-end-position))
        (when (< (setq c (+ a prsj-group-tab)) e)
          (save-excursion
            (if (re-search-forward " *:" e t)
                (setq e (1- (match-end 0)))
              )))
        (while (= (char-after) 32)
          (forward-char 1)
          )
        (prsj-make-hilite-bar 'prsj-hilight-bar (point) e)
        (prsj-save-window-pos)
        ))))

(defun prsj-make-hilite-bar (s a e)
  (let (b)
    (if (and (boundp s) (setq b (eval s)))
        (move-overlay b a e)
        (overlay-put
           (set s (make-overlay a e))
           'face '(:background "grey90" :foreground "blue")
           ))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick Search

(defun prsj-qsearch ()
  (interactive)
  (setq prsj-qs-str
        (cond ((member last-command-event '(backspace 127))
               (substring prsj-qs-str 0 (max 0 (1- (length prsj-qs-str))))
               )
              ((eq last-command-event 'delete)
               ""
               )
              (t
               (concat prsj-qs-str (char-to-string last-command-event))
               )))
  (prsj-qs-next 0)
  )

(defun prsj-qs-clear ()
  (when prsj-qs-face
    (delete-overlay prsj-qs-face)
    )
  (setq prsj-qs-face nil)
  (setq prsj-qs-pos nil)
  (setq prsj-qs-str "")
  (setq prsj-qs-len 0)
  )

(defun prsj-qs-find (s f p)
  (save-excursion
    (let (r fn beg end start limit)
      (setq s (concat
               "^[[:space:]]*\\([^[:space:]]*[/\\]\\)?\\("
               (regexp-quote s)
               "\\)[^/\\[:space:]]*\\([[:space:]]\\|$\\)"
               ))

      (prsj-goto-line prsj-group-top)
      (setq beg (point))
      (setq end (point-max))
      (goto-char (max p beg))

      (if (>= f 0)
          (setq fn 're-search-forward
                start beg
                limit end
                )
          (setq fn 're-search-backward
                start end
                limit beg
                ))

      (catch 'loop
        (while t
          (beginning-of-line (max 1 (1+ f)))
          (cond ((funcall fn s limit t)
                 (throw 'loop (match-beginning 2))
                 )
                (r
                 (throw 'loop nil)
                 )
                ((setq r t)
                 (goto-char start)
                 )))))))

(defun prsj-qs-next (f)
  (let (k l p a e n s)
    (setq p prsj-qs-pos)
    (setq l prsj-qs-len)
    (setq s prsj-qs-str)
    (prsj-qs-clear)

    (setq k (length s))
    (if (= k 0)
        (setq l k)
        (progn
          (if (setq n (prsj-qs-find s f (or p (point))))
              (setq p n l k)
              (setq s (substring s 0 l))
              )
          (message "Quick search: %s" s)
          ))

    (when p
      (goto-char (+ p l))
      (prsj-set-hilite-bar)
      (when (> l 0)
        (setq prsj-qs-face (make-overlay p (+ p l)))
        (overlay-put prsj-qs-face 'face '(:background "white" :box "black"))

        (setq prsj-qs-pos p)
        (setq prsj-qs-len l)
        (setq prsj-qs-str s)

        (when (setq e (read-key-sequence nil))
          (setq e (listify-key-sequence e))
          (setq  unread-command-events (nconc e unread-command-events))
          (unless (lookup-key prsj-browse-map (vconcat e) t)
            (prsj-qs-clear)
            ))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prosjekt-config.el ends here
