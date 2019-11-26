;;; cAndCpp_conf --- Holds the configurations for C / C++

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
(setq-default c-indent-tabs-mode nil
	      c-indent-level 4
	      c-argdecl-indent 0
	      backward-delete-function nil)

;; - Indent or complete with <TAB>
(setq tab-always-indent 'complete)

(defun my-c-lineup-arglist-lambda (langelem)
  "Line up lambda."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "{")
      '+)))

(c-add-style "Iv O'Style"
	     '("linux"
	       (indent-tabs-mode nil)
	       (c-basic-offset . 4)
	       (c-offsets-alist . ((inline-open . +)
				   (brace-list-open . +)
				   (statement-case-open . +)
				   (substatement-open . 0)
				   (block-open . +)
				   (case-label . +)
				   (arglist-cont-nonempty
				    (my-c-lineup-arglist-lambda c-lineup-arglist))))))

(defun my-c-mode-hook()
  (c-set-style "Iv O'Style")
  (auto-fill-mode)
  (subword-mode 1))


;; - Set C++ style
(defun my-c++-mode-hook ()
  (c-set-style "Iv O'Style")
  (auto-fill-mode)
  (subword-mode 1))



;; - Hook the styles
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;========================

(provide 'cAndCpp_conf)
;;; cAndCpp_conf.el ends here
