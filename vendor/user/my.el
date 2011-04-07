(defvar estarter-custom-js-globals
  (list "ROCHE" "GRONO" "GDG" "GENE" "Λ")
  "List of possible global names for JS pack")

;; (setq estarter-custom-js-globals (list "ROCHE" "GRONO" "GDG" "GENE" "Λ"))

;;;###autoload
(defun estarter-custom-js-get-global ()
  (save-excursion
    (let ((case-fold-search nil))
      (or
        (some (lambda (name)
            (if (or
                (search-backward name nil t 1)
                (search-forward name nil t 1))
              name)) estarter-custom-js-globals)
        (car (last estarter-custom-js-globals))))))
