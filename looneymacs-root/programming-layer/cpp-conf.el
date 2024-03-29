;;; cpp-conf --- Holds the configurations for C / C++

;;; Commentary:
;; - C/C++ configurations and packages to make work
;; with these languages easier and more pleasant (not
;; that it could be any more than it already is :-) )



;;; Code:
;;==========================
;;= C / C++ Configurations =
;;==========================
;; - Set styles for C/C++ and hook them



;;========================
;;= Set C and C++ Styles =
;;=     and Hook them    =
;;========================



;; - Set C style
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq-default c-indent-level 4
	      c-argdecl-indent 0
	      backward-delete-function nil)

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))

(defun my-c-lineup-arglist-lambda (langelem)
  "Line up lambda."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "{")
      '+)))

(c-add-style "custom-c-style"
	     '((c-basic-offset . 4)
	       (c-offsets-alist . ((inline-open . +)
				   (brace-list-open . +)
				   (statement-case-open . +)
				   (substatement-open . 0)
				   (block-open . +)
				   (case-label . +)
				   (arglist-cont-nonempty
				    (my-c-lineup-arglist-lambda c-lineup-arglist))))))

(defun my-c-mode-hook()
  (c-set-style "custom-c-style")
  (auto-fill-mode)
  (my-indent-setup))


;; ;; - Set C++ style
(defun my-c++-mode-hook ()
  (c-set-style "custom-c-style")
  (auto-fill-mode)
  (my-indent-setup))



;; - Hook the styles
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;========================

(provide 'cpp-conf)
;;; cpp-conf.el ends here
