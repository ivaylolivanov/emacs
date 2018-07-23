;;==========================
;;= C / C++ Configurations =
;;==========================



;;========================
;;= Set C and C++ Styles =
;;=     and Hook them    =
;;========================



;; - Set C style
(setq-default c-indent-tabs-mode t
	      c-indent-level 4
	      c-argdecl-indent 0
	      c-tab-always-indent t
	      backward-delete-function nil)

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
  (c-toggle-auto-hungry-state 0)
  (subword-mode 1)
  (c-toggle-syntactic-indentation 1)
  (c-hungry-delete 1))


;; - Set C++ style
(defun my-c++-mode-hook ()
  (c-set-style "Iv O'Style")
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 0)
  (subword-mode 1)
  (c-toggle-syntactic-indentation 1))


;; - Hook the styles
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;========================


;; TODO Add company and irony and


(provide 'cAndCpp_conf)
