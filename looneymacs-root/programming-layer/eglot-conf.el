;;; cs-conf --- Eglot configurations

;;; Commentary:
;;========================
;;= Eglot configurations =
;;========================
;; Heavily influenced by
;; https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-csharp.el
;;
;; Content:
;; - Constants
;; - Language independent variables
;; - Setup omnisharp-roslyn
;;     - C# Customizable variables
;;     - Omnisharp-roslyn related functions
;; - Setup rust-analyzer
;;     - Rust Customizable variables
;;     - Rust analyzer related functions
;; - Eglot mode configurations
;; - Eglot keybinds
;; - Utilities
;;     - Gunzip utils
;;     - Unzip utils


;;; Code:
;; - Constants
(defconst powershell-unzip-script "powershell -noprofile -noninteractive \
-nologo -ex bypass Expand-Archive -path '%s' -dest '%s' -Force"
  "Powershell script to unzip file.")

(defconst bash-unzip-script
  "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"
  "Unzip script to unzip file.")

(defconst gunzip-script "gzip -d %1$s"
  "Script to decompress a gzippped file with gzip.")

;; - Language independent variables
(defcustom language-servers-dir
  (expand-file-name (concat (file-name-as-directory conf-root-dir) "language-servers"))
  "Directory in which the servers will be installed."
  :risky t
  :type 'directory
  :group 'eglot)



;;==========================
;;= Setup omnisharp-roslyn =
;;==========================
;; - C# Customizable variables
(defcustom omnisharp-roslyn-unpack-dir
  (expand-file-name "omnisharp-roslyn" language-servers-dir)
  "The path where omnisharp-roslyn .zip archive will be extracted."
  :group 'eglot-omnisharp
  :type 'file)

(defcustom omnisharp-roslyn-zip-store-path
  (expand-file-name "omnisharp-roslyn.zip" omnisharp-roslyn-unpack-dir)
  "The path where omnisharp-roslyn .zip archive will be stored."
  :group 'eglot-omnisharp
  :type 'file)

(defcustom omnisharp-roslyn-url
  (concat "https://github.com/omnisharp/omnisharp-roslyn/releases/latest/download/"
          (cond ((eq system-type 'windows-nt)
                 ; On Windows we're trying to avoid a crash starting 64bit .NET PE binaries in
                 ; Emacs by using x86 version of omnisharp-roslyn on older (<= 26.4) versions
                 ; of Emacs. See https://lists.nongnu.org/archive/html/bug-gnu-emacs/2017-06/msg00893.html"
                 (if (and (string-match "^x86_64-.*" system-configuration)
                          (version<= "26.4" emacs-version))
                     "omnisharp-win-x64.zip"
                   "omnisharp-win-x86.zip"))

                ((eq system-type 'darwin)
                 (if (string-match "aarch64-.*" system-configuration)
                     "omnisharp-osx-arm64-net6.0.zip"
                   "omnisharp-osx-x64-net6.0.zip"))

                ((and (eq system-type 'gnu/linux)
                      (or (eq (string-match "^x86_64" system-configuration) 0)
                          (eq (string-match "^i[3-6]86" system-configuration) 0)))
                 "omnisharp-linux-x64-net6.0.zip")

                (t "omnisharp-mono.zip")))
  "Build appropriate download URL for omnisharp-roslyn."
  :group 'eglot-omnisharp
  :type 'string)

(defcustom language-server-csharp-path
  nil
  "The path to the OmniSharp Roslyn language-server binary."
  :group 'eglot-omnisharp
  :type '(string :tag "Single string value or nil"))



;; - Omnisharp-roslyn related functions
(defun omnisharp-roslyn-download ()
  "Download omnisharp-roslyn archive, unzip it and set as executable."
  (url-copy-file omnisharp-roslyn-url
                 omnisharp-roslyn-zip-store-path 1)
  (unzip omnisharp-roslyn-zip-store-path
         omnisharp-roslyn-unpack-dir)
  (unless (eq system-type 'windows-nt)
    (let ((omnisharp-executable
           (expand-file-name "OmniSharp" omnisharp-roslyn-unpack-dir)))
      (set-file-modes omnisharp-executable #o755))))

(defun get-csharp-language-server-path ()
  "Resolve path to use to start the csharp language server."
  (if language-server-csharp-path
      (executable-find language-server-csharp-path)
    (let ((server-dir omnisharp-roslyn-unpack-dir))
        (when (file-exists-p server-dir)
          (expand-file-name
           (concat (file-name-as-directory server-dir)
                   (cond ((eq system-type 'windows-nt) "OmniSharp.exe")
                         (t "OmniSharp")))
           server-dir)))))

(defun eglot-setup-omnisharp ()
  "Function to download, setup and make omnisharp-roslyn ready for use."
  (interactive)
  (unless (file-exists-p omnisharp-roslyn-unpack-dir)
    (make-directory omnisharp-roslyn-unpack-dir))
  (omnisharp-roslyn-download))

(defun eglot-csharp-select-server ()
  "Function to select csharp LSP server based on personal preferencies.
1) - charp-ls
2) - omnisharp-roslyn
3) - Shows a reminder to install something."
  (cond ((executable-find "csharp-ls") (list "csharp-ls"))
        ((executable-find (get-csharp-language-server-path))
         (list (get-csharp-language-server-path) "-lsp"))
        (t (progn
             (error "ERROR: C# LSP server not found, install one!")
             nil))))

;;========================



;;=======================
;;= Setup rust-analyzer =
;;=======================
;; - Rust customizable variables
(defcustom rust-analyzer-unpack-dir
  (expand-file-name "rust-analyzer" language-servers-dir)
  "The path where rust-analyzer archive will be extracted."
  :group 'eglot-rust
  :type 'file)

(defcustom rust-analyzer-archive-store-path
  (expand-file-name "rust-analyzer.gz" rust-analyzer-unpack-dir)
  "The path where rust-analyzer archive will be stored."
  :group 'eglot-rust
  :type 'file)

(defcustom rust-analyzer-url
  (format "https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/%s"
          (pcase system-type
            ('gnu/linux "rust-analyzer-x86_64-unknown-linux-gnu.gz")
            ('darwin (if (string-match "^aarch64-.*" system-configuration)
                         "rust-analyzer-aarch64-apple-darwin.gz"
                       "rust-analyzer-x86_64-apple-darwin.gz"))
            ('windows-nt "rust-analyzer-x86_64-pc-windows-msvc.gz")))
  "Automatic download url for Rust Analyzer"
  :type 'string
  :group 'eglot-rust)

(defcustom language-server-rust-path
  nil
  "The path to the Rust Analyzer language-server binary."
  :group 'eglot-rust
  :type '(string :tag "Single string value or nil"))



;; - Rust analyzer related functions
(defun rust-analyzer-download ()
  "Download rust-analyzer archive, unzip it and set it as executable."
  (when (not (file-exists-p rust-analyzer-unpack-dir))
    (mkdir rust-analyzer-unpack-dir))
  (url-copy-file rust-analyzer-url
                 rust-analyzer-archive-store-path 1)
  (gunzip rust-analyzer-archive-store-path)
  (let ((rust-analyzer-executable
         (expand-file-name "rust-analyzer" rust-analyzer-unpack-dir)))
    (set-file-modes rust-analyzer-executable #o755)))

(defun setup-rust-analyzer ()
  "Resolve the path to rust-analyzer and add it to `exec-path`."
  (if language-server-rust-path
      (executable-find language-server-rust-path)
    (let ((server-dir rust-analyzer-unpack-dir))
      (if (file-exists-p server-dir)
          (expand-file-name "rust-analyzer" server-dir)
        (progn
          (rust-analyzer-download)
          (setup-rust-analyzer)))
      (add-to-list 'exec-path server-dir))))

;;=======================



;;=============================
;;= Eglot mode configurations =
;;=============================
(use-package eglot
  :ensure t
  :defer
  :init
  (setup-rust-analyzer)
  :hook
  (csharp-mode . eglot-ensure)
  (c-mode      . eglot-ensure)
  (c++-mode    . eglot-ensure)
  (rust-mode   . eglot-ensure)
  (python-mode . eglot-ensure)
  (sh-mode     . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(csharp-mode . ,(eglot-csharp-select-server)))
  (remove-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode))

;; ============================



;; ==================
;; = Eglot keybinds =
;; ==================
(global-set-key (kbd "C-c r") 'eglot-rename)


;;=============
;;= Utilities =
;;=============
;; - Gunzip utils
(defcustom gunzip-script-command
  (lambda ()
    (cond ((executable-find "gzip") gunzip-script)
          (t nil)))
  "The script to decompress a gzipped file.
Should be a format string with one argument for the file to be decompressed
in place."
  :group 'eglot-utils
  :type 'string)

(defun gunzip (gz-file)
  "Decompress GZ-FILE in place."
  (unless gunzip-script-command
    (error "Unable to find `gzip' on the path, please either customize `gunzip-script-command' or manually decompress %s" gz-file))
  (shell-command (format (funcall gunzip-script-command) gz-file)))



;; - Unzip utils
(defcustom unzip-script
  (lambda ()
    (cond ((executable-find "powershell") powershell-unzip-script)
          ((executable-find "unzip") bash-unzip-script)
          (t nil)))
  "The script for unzipping."
  :group 'eglot-utils
  :type 'string)

(defun unzip (zip-file dest)
  "Unzip ZIP-FILE to DEST."
  (unless unzip-script
    (error "Unable to find `unzip' or `powershell' on the path, please customize `unzip-script'"))
  (shell-command (format (funcall unzip-script) zip-file dest)))

;;=============

(provide 'eglot-conf)
;;; eglot-conf.el ends here
