;;=================================
;;= 1. Greeting and verions check =
;;=================================

(defvar master (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(message "Hello, %s sama, glad you are here again!\nDo your best :\)" master)

(when (version< emacs-version "24.4")
  (error "Please upgrade me, %s sama, my version is: %s and it should be at least: 24.4" master emacs-version))


;;==============================
;;=  2. Load newest byte code  =
;;==============================

(setq load-prefer-newer t)

;;------------------------------


;;====================================================
;;=  3. Manage and craete structure for the configs  =
;;====================================================

(defvar conf-root-dir
  (expand-file-name "M00ns7rucksEmacs" user-emacs-directory)
  "Contains settings and configurations.")

(defvar languageLayer-dir
  (expand-file-name "LanguageLayerSettings" conf-root-dir)
  "Contains settings and configrations of programming languages.")

;; Create folder for the configurations
(unless (file-exists-p conf-root-dir)
  (make-directory conf-root-dir)
  (message "The root directory was created"))
(unless (file-exists-p languageLayer-dir)
  (make-directory languageLayer-dir)
  (message "The directory for programming languages was created"))

;; Write backups in its onw directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "Backup")))))


;; - Include the directories in the load path
(add-to-list 'load-path conf-root-dir)
(add-to-list 'load-path languageLayer-dir)


;;====================================================


;;=============================
;;=  4. Configure the backup  =
;;=============================

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

;;=============================


;;==============================================================
;;=  5. Temporarily reduce garbage collection during start up  =
;;=         Purcell's emacs settings(file: init.el)            =
;;==============================================================

(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;==============================================================


;;====================================
;;=  6. Call the core configurations =
;;====================================

(require 'core)

;;====================================


(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-mode flycheck yasnippet-snippets yasnippet neotree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
