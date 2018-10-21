;;; core.el --- Houses core settings of the configurations

;;; Commentary:
;;=======================
;;= Core configurations =
;;=======================
;; - Initialize melpa
;; - Set UTF-8 as default encoding
;; - Discard useless UI things
;;     - Set theme
;;     - Dashboard
;; - Install Use-package
;; - NeoTree, UndoTree, Dired, Recentf, Ediff and Eldoc
;; - Go-to-address mode
;; - Warn by oppening large files
;; - Revert buffers automatically
;; - Delete selection with keypress
;; - Highlight current line
;; - Highlight trailing whitespaces
;; - Delete trailing whitespaces before save
;; - Save cursor position
;; - Newline at end of the files
;; - Revert buffer with <F6>
;; - Windmove
;; - Volatile Highlight
;; - Ivy
;; - Swiper
;; - Counsel

;;; Code:



;;====================
;;= Initialize melpa =
;;====================
(require 'melpa)
;;====================



;;===================================
;;= - Set UTF-8 as default encoding =
;;===================================
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;;===================================



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

;; - Turn off interfaces
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; - Remove bells
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; - Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; - Make the frame title useful
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
					    (abbreviate-file-name (buffer-file-name))
					  "%b"))))

(display-time-mode 1) ; Show time in the mode line

(fset 'yes-or-no-p 'y-or-n-p) ; Short answers only (y / n)


;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)
;; - Display ibuffer in new window
(setq ibuffer-use-other-window t)


;; - Confirmation for quitting emacs with y/ n
(setq confirm-kill-emacs                 'y-or-n-p
      confirm-nonexistent-file-or-buffer t)


;; - Set font
(setq default-frame-alist '((font . "Inconsolata-13")))
(set-face-attribute 'italic nil
		    :family "Inconsolata")


;; - Set theme
 (use-package spacemacs-theme
   :ensure t
   :init
   (load-theme 'spacemacs-dark t))

;; - Dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Welcome, Make your worth it!")
  (setq dashboard-startup-banner "~/Pictures/images.png")

  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))

  (dashboard-setup-startup-hook)
  )
;;==============================



;;=====================================
;;= Install Use-Package and enable it =
;;=====================================
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
;;=====================================



;;============================================
;;= Configure NeoTree, Dired, Recentf, Ediff =
;;=                and Eldoc                 =
;;============================================
;; - Neotree
;; - Undo-Tree
;; - Dired
;; - Recentf
;; - Ediff
;; - Eldoc



;; - Set key for Neo-Tree
(global-set-key [f8] 'neotree-toggle)


;; - Undo-Tree
(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))


;; - Dired
(use-package dired
  :ensure nil
  :config
  (setq
   dired-dwim-target t                    ; if another Dired buffer is visible in another window, use that directory as target for Rename/Copy
   dired-recursive-copies 'always         ; "always" means no asking
   dired-recursive-deletes 'top           ; "top" means ask once for top level directory
   dired-listing-switches "-lha"          ; human-readable listing
   )
  (add-hook 'dired-mode-hook 'auto-revert-mode)       ; automatically refresh dired buffer on changes


  ;; If it is not Windows, use the following listing switches
  (when (not (eq system-type 'windows-nt))
    (setq dired-listing-switches "-lha --group-directories-first")))


;; - Recentf
(use-package recentf
  :ensure t
  :config
  (setq ;;recentf-save-file (expand-file-name "recentf" backup-directory-alist)
        recentf-max-saved-items 5000
        recentf-max-menu-items 30
        recentf-auto-cleanup 'never) ;; Because of problems with remote files
  (recentf-mode +1))

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


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)


;; - Delete the selection with a keypress
(delete-selection-mode t)


;; - Highlight current line
(global-hl-line-mode +1)
;; - Highlight trailing whitespaces
(add-hook 'prog-mode-hook
	  (lambda () (interactive)
	    (setq show-trailing-whitespace 1)))


;; - Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; - Save the cursor position
(setq-default save-place t)
;; - Newline at end of the file
(setq require-final-newline t)


;; - Bind revert-buffer command to <F6>
 (global-set-key
  (kbd "<f6>")
  (lambda (&optional force-reverting)
    "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
    (interactive "P")
    ;;(message "force-reverting value is %s" force-reverting)
    (if (or force-reverting (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm)
      (error "The buffer has been modified"))))



;;=============
;; - Windmove =
;;=============
(use-package windmove
  :bind  (("C-x k" . windmove-up)
	  ("C-x j" . windmove-down)
	  ("C-x h" . windmove-left)
	  ("C-x l" . windmove-right)))
;;=============



;;========================
;; - Volatile highlights =
;;========================
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))
;;========================



;;=========
;;= - Ivy =
;;=========
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))
;;=========



;;============
;;= - Swiper =
;;============
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))
;;============



;;=============
;;= - Counsel =
;;=============
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-c l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))
;;=============



;;==================================
;;=    Call the other settings     =
;;= (something like main function) =
;;==================================
(require 'prog_base)

;;==================================

(provide 'core)
