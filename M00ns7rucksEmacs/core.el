;;=======================
;;= Core configurations =
;;=======================
;; - Set UTF-8 as default encoding
;; - Discard useless UI things
;; - NeoTree, Dired, Recentf, Ediff and Eldoc
;; - Go-to-address mode
;; - Warn by oppening large files
;; - Highlight current line
;; - Show line numbers
;; - Show trailing whitespaces
;; - Save cursor positon



;; - Set UTF-8 as default encoding
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))


;;==============================
;; - Discard useless UI things =
;;==============================
(setq-default truncate-lines t)          ; Truncate lines
(setq fringes-outside-margins   t
      use-package-always-ensure t)
(setq inhibit-startup-message t)         ; Remove splash screen
(blink-cursor-mode -1)                   ; Disable blinking cursor
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1) ; Disable scrollbar
(size-indication-mode t)

;; - Turn off mouse interface
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; - Remove bells
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; - Make the frame title useful
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
					    (abbreviate-file-name (buffer-file-name))
					  "%b"))))
(display-time-mode 1) ; Show time in the mode line

(fset 'yes-or-no-p 'y-or-n-p) ; Short answers only (y / n)


;; - Display ibuffer in new window
(setq ibuffer-use-other-window t)


;; - Confirmation for quitting emacs with y/ n
(setq confirm-kill-emacs                 'y-or-n-p
      confirm-nonexistent-file-or-buffer t)

;;==============================



;;============================================
;;= Configure NeoTree, Dired, Recentf, Ediff =
;;=                and Eldoc                 =
;;============================================
;; - Neotree
;; - Dired
;; - Recentf
;; - Ediff
;; - Eldoc



;; - Set key for Neo-Tree
;; (require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; - Dired
(setq
 dired-dwim-target t                    ; if another Dired buffer is visible in another window, use that directory as target for Rename/Copy
 dired-recursive-copies 'always         ; "always" means no asking
 dired-recursive-deletes 'top           ; "top" means ask once for top level directory
 dired-listing-switches "-lha"          ; human-readable listing
 )
(add-hook 'dired-mode-hook 'auto-revert-mode)       ; automatically refresh dired buffer on changes


;; If it is not Windows, use the following listing switches
(when (not (eq system-type 'windows-nt))
  (setq dired-listing-switches "-lha --group-directories-first"))


;; - Recentf
(setq
 recentf-max-menu-items 30
 recentf-max-saved-items 5000)


;; - Ediff
  (setq ediff-diff-options "-w"
	ediff-split-window-function 'split-window-horizontally
	ediff-window-setup-function 'ediff-setup-windows-plain)


;; - Manage Eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;============================================



;; - Hook Go-to-address-mode
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)


;; - Warn when oppening larg file
(setq large-file-warning-threshold 100000000)


;; - Highlight current line
(global-hl-line-mode)


;; - Show number of line in programming modes
(add-hook 'prog-mode-hook 'linum-mode)


;; - Highlight trailing whitespaces
(add-hook 'prog-mode-hook
	  (lambda () (interactive)
	    (setq show-trailing-whitespace 1)))


; - Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; - Save the cursor position
(setq-default save-place t)



;;==================================
;;=    Call the other settings     =
;;= (something like main function) =
;;==================================
(require 'melpa)
(require 'prog_base)

;;==================================

(provide 'core)
