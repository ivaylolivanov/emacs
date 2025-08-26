;; -*- lexical-binding: t; -*-
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
(message "Hello, %s, glad you are here again!\nDo your best :)" master)

(when (version< emacs-version "26.1")
  (error "Please upgrade me, %s, my version is: %s and it should be at least: 26.1" master emacs-version))
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
  (expand-file-name "looneymacs-root" user-emacs-directory)
  "Contains settings and configurations.")

(defvar programming-layer-dir
  (expand-file-name "programming-layer" conf-root-dir)
  "Contains settings and configurations of programming utilities.")

(defvar custom-packages-dir
  (expand-file-name "custom-packages" conf-root-dir)
  "Contains custom packages.")

;; Create folder for the configurations
(unless (file-exists-p conf-root-dir)
  (make-directory conf-root-dir)
  (message "The root directory was created"))
(unless (file-exists-p programming-layer-dir)
  (make-directory programming-layer-dir)
  (message "The directory for programming languages was created"))
(unless (file-exists-p custom-packages-dir)
  (make-directory custom-packages-dir)
  (message "The directory for custom packages was created"))

;; Org-roam directory
(defvar org-roam-storage-dir
  (expand-file-name "Documents/org-roam"
                    (getenv "HOME"))
  "Directory for org-roam storage.")
(make-directory org-roam-storage-dir :parents)

;; Dashboard banner location
(defvar dashboard-custom-banner-path
  (concat (getenv "HOME")
	  "/Pictures/dashboard_banner.png")
  "The variable holds the path to my default banner logo.")

;; - Write backups in its own directory and configure backups
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
(add-to-list 'load-path programming-layer-dir)
(add-to-list 'load-path custom-packages-dir)
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
 '(ispell-dictionary nil)
 '(org-roam-db-location "/home/ivaylo/.emacs.d/org-roam.db")
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(company counsel cquery csharp-mode doom-modeline editorconfig eglot
             eldoc elisp-slime-nav elpy erc faceup flycheck
             flycheck-rust flymake htmlize idlwave ivy js2-mode
             jsonrpc lisp-mode lsp-clangd lsp-javascript-flow
             lsp-javascript-typescript lsp-python lsp-ui neotree org
             org-roam paredit project py-autopep8 python racer
             rainbow-delimiters rainbow-mode rust-mode seq slime
             soap-client spacemacs-theme spinner sqlite3 track-changes
             use-package verilog-mode which-key window-tool-bar xref
             yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
