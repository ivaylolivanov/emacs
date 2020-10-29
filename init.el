;;; init.el --- M00ns7ruck's configurations

;;; Commentary:
;; Personal Emacs configurations

;;; Code:
;; - Greet and check versions
;; - Load newest byte code
;; - Temporal reduction of garbage collection (during start)
;; - Create the files / directories of the configuration
;; - Call the core of the configurations



;;=================================
;;= 1. Greeting and verions check =
;;=================================
(defvar master (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(message "Hello, %s sama, glad you are here again!\nDo your best :)" master)

(when (version< emacs-version "26.1")
  (error "Please upgrade me, %s sama, my version is: %s and it should be at least: 26.1" master emacs-version))
;;=================================



;;==============================
;;=  2. Load newest byte code  =
;;==============================
(setq load-prefer-newer t)
;;==============================



;;==============================================================
;;=  3. Temporarily reduce garbage collection during start up  =
;;==============================================================
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold 50000000)
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))
;;==============================================================



;;====================================================
;;=  4. Manage and create structure for the configs  =
;;====================================================

(defvar conf-root-dir
  (expand-file-name "LooneyMacs-Root" user-emacs-directory)
  "Contains settings and configurations.")

(defvar programmingLayer-dir
  (expand-file-name "ProgrammingLayer" conf-root-dir)
  "Contains settings and configurations of programming utilities.")

;; Create folder for the configurations
(unless (file-exists-p conf-root-dir)
  (make-directory conf-root-dir)
  (message "The root directory was created"))
(unless (file-exists-p programmingLayer-dir)
  (make-directory programmingLayer-dir)
  (message "The directory for programming languages was created"))

;; Org agenda directory
(defvar org-agenda-dir
  (concat (getenv "HOME") "/Documents/org_agenda/")
  "Directory for org agenda.")
(make-directory org-agenda-dir :parents)

;; Org notes directory
(defvar org-notes-dir
  (concat (getenv "HOME") "/Documents/notes/")
  "Directory for quick notes.")
(make-directory org-notes-dir :parents)

;; Dashboard banner location
(defvar dashboard-custom-banner-path
  (concat (getenv "HOME")
	  "/Pictures/dashboard_banner.png")
  "The variable holds the path to my default banner logo.")

;; - Write backups in its onw directory
;; And configure backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "Backup")))))

(setq
 make-backup-files t        ; backup a file the first time it is saved
 backup-by-copying t        ; copy the current file into backup directory
 version-control t          ; version numbers for backup files
 delete-old-versions t      ; delete unnecessary versions
 kept-old-versions 5        ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9        ; newest versions to keep when a new numbered backup is made (default: 2)
 auto-save-default t        ; auto-save every buffer that visits a file
 auto-save-timeout 20       ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200     ; number of keystrokes between auto-saves (default: 300)
 vc-make-backup-files t     ; make backups of files, even when they're in version control
 )

(setq auto-save-list-file-prefix nil)    ; Disable creation of the directory auto-save-list


;; - Include the directories in the load path
(add-to-list 'load-path conf-root-dir)
(add-to-list 'load-path programmingLayer-dir)
;;====================================================



;;====================================
;;=  5. Call the core configurations =
;;====================================
(require 'core)
;;====================================

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-project-whitelist (quote ("^/home/ivo/Programming/Projects/IT_Project/$")))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (dap-mode company csharp-mode js2-mode magit spinner flycheck-rust racer rust-mode lisp-mode elisp-slime-nav paredit counsel slime py-autopep8 elpy lsp-ui lsp-javascript-flow lsp-javascript-typescript lsp-python cquery lsp-clangd spacemacs-theme rainbow-mode rainbow-delimiters use-package ivy yasnippet-snippets neotree flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
