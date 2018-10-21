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

(when (version< emacs-version "24.4")
  (error "Please upgrade me, %s sama, my version is: %s and it should be at least: 24.4" master emacs-version))
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
  (expand-file-name "M00ns7rucksEmacs" user-emacs-directory)
  "Contains settings and configurations.")

(defvar languageLayer-dir
  (expand-file-name "LanguageLayerSettings" conf-root-dir)
  "Contains settings and configrations of programming languages.")

(defvar customs-file
  (expand-file-name "custom.el" conf-root-dir)
  "Contains settings donw via the UI.")

;; Create folder for the configurations
(unless (file-exists-p conf-root-dir)
  (make-directory conf-root-dir)
  (message "The root directory was created"))
(unless (file-exists-p languageLayer-dir)
  (make-directory languageLayer-dir)
  (message "The directory for programming languages was created"))
(when (file-exists-p customs-file)
  (load customs-file))

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
(add-to-list 'load-path languageLayer-dir)
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
 '(package-selected-packages
   (quote
    (lsp-ui lsp-javascript-flow lsp-javascript-typescript lsp-mode lsp-python cquery lsp-clangd spacemacs-theme rainbow-mode rainbow-delimiters use-package ivy yasnippet-snippets neotree flycheck company-lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
