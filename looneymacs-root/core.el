;;; core.el --- Houses core settings of the configurations

;;; Commentary:
;;=======================
;;= Core configurations =
;;=======================
;; - Initialize melpa
;; - Install Use-package
;; - Set UTF-8 as default encoding
;; - Discard useless UI things
;; - Smooth mouse scrolling
;; - Dired, Recentf, Ediff and Eldoc
;; - Go-to-address mode
;; - Warn by oppening large files
;; - Revert buffers automatically
;; - Delete selection with keypress
;; - Highlight current line
;; - Highlight trailing whitespaces
;; - Delete trailing whitespaces before save
;; - Save cursor position
;; - Newline at end of the files
;; - Windmove
;; - Volatile Highlight
;; - Ivy
;; - Swiper
;; - Counsel
;; - Start Emacs as server

;;; Code:
;;====================
;;= Initialize melpa =
;;====================
(require 'gnutls)
(require 'melpa)
;;====================



;;=====================================
;;= Install Use-Package and enable it =
;;=====================================
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
;;=====================================



;;===================================
;;= - Set UTF-8 as default encoding =
;;===================================
(set-charset-priority 'unicode)
(setq system-time-locale "C")
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
;; - Truncate lines
(setq-default truncate-lines t)
;; - Remove splash screen
(setq inhibit-startup-message t)
;; - Disable blinking cursor
(blink-cursor-mode -1)
;; - Remove bells
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; - Set theme
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))
(use-package haki-theme
  :custom-face
  (haki-region ((t (:background "#2e8b57" :foreground "#ffffff"))))
  (haki-highlight ((t (:background "#fafad2" :foreground "#000000"))))
  :config
  ;; TODO: Download the nerd-font, then uncomment this
  ;; (setq
  ;;  haki-heading-font "Comic Mono"
  ;;  haki-sans-font "Iosevka Comfy Motion"
  ;;  haki-title-font "Impress BT"
  ;;  haki-link-font "VictorMono Nerd Font"
  ;;  haki-code-font "Maple Mono")
  ;; (load-theme 'haki t)
  )
;; - Beautify UI
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(setq-default mode-line-format nil)
(set-face-attribute 'default nil :font "Inconsolata" :height 170)
(set-fringe-mode (/ (- (frame-pixel-width) (* 120 (frame-char-width))) 2))
(set-window-fringes nil 0 0)
;; - Short answers only (y / n)
(fset 'yes-or-no-p 'y-or-n-p)
;; - Confirmation for quitting emacs with y/n
(setq confirm-kill-emacs 'y-or-n-p
      confirm-nonexistent-file-or-buffer nil)
;; - Enable winner mode
(when (fboundp 'winner-mode)
      (winner-mode 1))
;; - Dashboard
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Welcome, Make your day worth it!")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (if (file-exists-p dashboard-custom-banner-path)
      (setq dashboard-startup-banner dashboard-custom-banner-path)
    (setq dashboard-startup-banner 'logo))
  (setq dashboard-items '((projects . 5)
			  (recents  . 5)))
  (setq dashboard-startupify-list
        '(dashboard-insert-banner
          dashboard-insert-newline
          dashboard-insert-banner-title
          dashboard-insert-items
          dashboard-insert-init-info))
  (dashboard-setup-startup-hook))
;;==============================



;;============================
;;= Smoother mouse scrolling =
;;============================
(if (version< emacs-version "29.0.50")
    (progn
      (pixel-scroll-mode)
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
      (setq mouse-wheel-progressive-speed nil)
      (setq mouse-wheel-follow-mouse 't)
      (setq scroll-step 1))
  (setq pixel-scroll-precision-mode 1))
;;============================



;;==============================================
;;= Configure Dired, Recentf, Ediff, Eldoc and =
;;=                 Ibuffer                    =
;;==============================================
;; - Dired
;; - Recentf
;; - Ediff
;; - Eldoc
;; - Ibuffer



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


;; - Ibuffer
;; replace buffer-menu with ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))
;;============================================



;; - Hook Go-to-address-mode
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)


;; - Warn when oppening larg file
(setq large-file-warning-threshold 100000000)


;; Revert buffers automatically when underlying files are changed externally
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
  (global-set-key (kbd "C-c C-r") 'ivy-resume))
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
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))
;;=============



;;=======================
;;= Custom key bindings =
;;=======================
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "<f1>") 'eval-region)



;;=========================
;;= Start Emacs as server =
;;=========================
(server-start)



;;==================================
;;=    Call the other settings     =
;;= (something like main function) =
;;==================================
(require 'prog-base)
(require 'org-settings)
;;==================================

(provide 'core)
;;; core.el ends here
