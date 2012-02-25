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