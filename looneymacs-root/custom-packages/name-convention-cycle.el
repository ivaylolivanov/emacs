;;; package --- Summary
;;; Commentary:
;;; Simply cycle through a list of naming conventions:
;;;   * the_variable_name, aka. Snake case
;;;   * TheVariableName, Pascal case
;;;   * THE_VARIABLE_NAME, Upper snake case
;;;   * the-variable-name, Kebab case

;;; Code:

(defconst naming-conventions/snake-case 0)
(defconst naming-conventions/upper-snake-case 1)
(defconst naming-conventions/pascal-case 2)
(defconst naming-conventions/kebab-case 3)
(defconst naming-conventions/invalid -1)
(defconst naming-conventions
  (list naming-conventions/snake-case
        naming-conventions/upper-snake-case
        naming-conventions/pascal-case
        naming-conventions/kebab-case))

(defun snake-case-p (str)
  "Check if STR is in snake case format."
  (let ((case-fold-search nil))
        (string-match-p "^[a-z0-9_]*$" str)))

(defun upper-snake-case-p (str)
  "Check if STR is in upper snake case format."
  (let ((case-fold-search nil))
        (string-match-p "^[A-Z0-9_]*?$" str)))

(defun pascal-case-p (str)
  "Check if STR is in pascal case format."
  (let ((case-fold-search nil))
        (string-match-p "^[A-Z][A-Za-z0-9]*$" str)))

(defun kebab-case-p (str)
  "Check if STR is in kebab case format."
  (let ((case-fold-search nil))
        (string-match-p "^[a-z][a-z\-\/0-9]*$" str)))

(defun get-naming-convention-index (str)
  "Get the integer value of STR."
  (cond ((snake-case-p       str) naming-conventions/snake-case)
        ((upper-snake-case-p str) naming-conventions/upper-snake-case)
        ((pascal-case-p      str) naming-conventions/pascal-case)
        ((kebab-case-p       str) naming-conventions/kebab-case)
        (t                        naming-conventions/invalid)))

(defun get-next-naming-convention-index (index)
  "Use INDEX to get the next naming convention index."
  (let ((next-index (+ index 1)))
    (if (>= next-index (length naming-conventions))
        (elt naming-conventions 0)
      (elt naming-conventions next-index))))

(defun snake2upper-snake-case (str)
  "Convert STR from snake case to upper snake case."
  (upcase str))

(defun upper-snake-case2pascal-case (str)
  "Convert STR from upper snake case to pascal case."
  (mapconcat #'identity
             (mapcar #'capitalize
                     (split-string str "_"))
             ""))

(defun pascal-case2kebab-case (str)
  "Convert STR from pascal case to kebab case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string "\\([A-Za-z]\\)\\([A-Z0-9]\\)" "\\1-\\2" str))))

(defun kebab-case2snake-case (str)
  "Convert STR from kebab case to snake case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string "-" "_" str))))

(defun cycle-naming-convention (str)
  ""
  (let ((naming-convention-index-current (get-naming-convention-index str)))
    (if (> naming-convention-index-current naming-conventions/invalid)
        (progn
          (let ((naming-convention-index-next
                 (get-next-naming-convention-index
                  naming-convention-index-current)))
            (cond ((eq naming-convention-index-next naming-conventions/snake-case)
                   (kebab-case2snake-case str))
                  ((eq naming-convention-index-next naming-conventions/upper-snake-case)
                   (snake2upper-snake-case str))
                  ((eq naming-convention-index-next naming-conventions/pascal-case)
                   (upper-snake-case2pascal-case str))
                  ((eq naming-convention-index-next naming-conventions/kebab-case)
                   (pascal-case2kebab-case str))
                  ((eq naming-convention-index-next naming-conventions/invalid)
                   (message "Error: Invalid nameing convetion index")))
            ))
      (message "Error: Not supported naming convention"))))

(defun cycle-naming-convention-at-point ()
  "Apply cycle-naming-convention to the thing at point and replace it."
  (interactive)
  (let ((variable-name (thing-at-point 'sexp)))
    (let ((new-variable-name (cycle-naming-convention variable-name)))
      (delete-region
       (car (bounds-of-thing-at-point 'sexp))
       (cdr (bounds-of-thing-at-point 'sexp)))
      (insert new-variable-name))))

(global-set-key (kbd "C-c s") 'cycle-naming-convention-at-point)

(provide 'name-convention-cycle)
;;; name-convention-cycle.el ends here
