;;; cs-conf --- Eglot configurations

;;; Commentary:
;;========================
;;= Eglot configurations =
;;========================
;; - Setup omnisharp-roslyn
;;     - Constants
;;     - Customizable variables
;;     - Add language servers to exec-path
;;     - Omnisharp-roslyn functions
;; - Eglot mode configurations



;;; Code:
;;==========================
;;= Setup omnisharp-roslyn =
;;==========================
;; Heavily influenced by
;; https://github.com/emacs-lsp/lsp-mode/blob/master/clients/lsp-csharp.el

(require 'gnutls)
(require 'f)

;; - Constants
(defconst powershell-unzip-script "powershell -noprofile -noninteractive \
-nologo -ex bypass Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip file.")

(defconst bash-unzip-script
  "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"
  "Unzip script to unzip file.")

;; - Customizable variables
(defcustom language-servers-dir
  (expand-file-name (f-join (f-join conf-root-dir "language-servers")))
  "Directory in which the servers will be installed."
  :risky t
  :type 'directory
  :group 'eglot)

(defcustom omnisharp-roslyn-unpack-dir
  (f-join language-servers-dir "omnisharp-roslyn")
  "The path where omnisharp-roslyn .zip archive will be extracted."
  :group 'eglot-omnisharp
  :type 'file)

(defcustom omnisharp-roslyn-zip-store-path
  (f-join omnisharp-roslyn-unpack-dir "omnisharp-roslyn.zip")
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



;; - Add language servers to exec-path
;; (add-to-list 'exec-path language-servers-dir)



;; - Omnisharp-roslyn functions
(defun omnisharp-roslyn-download ()
  "Download omnisharp-roslyn archive, unzip it and set as executable."
  (url-copy-file omnisharp-roslyn-url
                 omnisharp-roslyn-zip-store-path 1)
  (unzip omnisharp-roslyn-zip-store-path
         omnisharp-roslyn-unpack-dir)
  (unless (eq system-type 'windows-nt)
    (let ((omnisharp-executable
           (f-join omnisharp-roslyn-unpack-dir "OmniSharp")))
      (set-file-modes omnisharp-executable #o755))))

(defun get-csharp-language-server-path ()
  "Resolve path to use to start the csharp language server."
  (interactive)
  (if language-server-csharp-path
      (executable-find language-server-csharp-path)
    (let ((server-dir omnisharp-roslyn-unpack-dir))
        (when (f-exists? server-dir)
          (f-join server-dir
                  (cond ((eq system-type 'windows-nt) "OmniSharp.exe")
                        (t "OmniSharp")))))))

(defun eglot-setup-omnisharp ()
  "Function to download, setup and make omnisharp-roslyn ready for use."
  (interactive)
  (unless (file-exists-p omnisharp-roslyn-unpack-dir)
    (make-directory omnisharp-roslyn-unpack-dir))
  (omnisharp-roslyn-download))



;; - Eglot package
(use-package eglot
  :ensure t
  :hook
  (csharp-mode . eglot-ensure)
  (c-mode      . eglot-ensure)
  (c++-mode    . eglot-ensure)
  (rust-mode   . eglot-ensure)
  (python-mode . eglot-ensure)
  (sh-mode     . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(csharp-mode . ,(list (get-csharp-language-server-path) "-lsp"))))



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
;;========================

(provide 'eglot-conf)
;;; eglot-conf.el ends here
