;;; package --- Summary
;;; Commentary:
;;; This package tries to get the current git root, then depending on the OS,
;;; either find 'build.ps' or 'build.sh'.  If such script is found, executes it.

;;; Code:

(defun run-and-capture-output (command &rest args)
  "Run a COMMAND and return its output as a list of lines."
  (with-temp-buffer
    (let* ((exit-code (apply 'call-process command nil t nil args))
           (output (split-string (buffer-string) "\n" t)))
      (unless (zerop exit-code)
        (error "Command failed with exit code %d: %s" exit-code command))
      output)))

(defun looney-get-git-root ()
  "Get the current Git root directory."
  (setq git-get-root "git rev-parse --show-toplevel")
  (setq current-directory (file-name-directory buffer-file-name))
  (if (fboundp 'vc-git-root)
      (expand-file-name (vc-git-root current-directory))
    (cond ((eq system-type 'gnu/linux)
           (shell-command-to-string git-get-root))
          ((eq system-type 'windows-nt)
           (run-and-capture-output git-get-root)))))

(defun looney-get-build-script (directory)
  "try to find depending on the current os build script in directory."
  (setq build-script-name
        (cond ((eq system-type 'windows-nt) "build.ps1")
              ((eq system-type 'gnu/linux)  "build.sh")))

  (unless (directory-name-p directory)
    (error (format "ERROR: %s does not exist" directory)))

  (setq build-script-files (directory-files-recursively directory build-script-name))
  (when (null build-script-files)
    (error (format "ERROR: No build script found in %s!" directory)))

  (setq build-script-abs-path (car build-script-files))
  (when (string-empty-p build-script-abs-path)
    (error (format "ERROR: No build script found in %s!" directory)))

  (unless (file-readable-p build-script-abs-path)
    (error (format "ERROR: %s is not readable!" build-script-abs-path)))

  (when (eq system-type 'gnu/linux)
    (unless (file-executable-p build-script-abs-path)
      (error (format "ERROR: %s is not executable!" build-script-abs-path))))

  (message build-script-abs-path))

(defun looney-build-current-project ()
  "Try to execute the build script for the current project."
  (interactive)
  (setq project-root (looney-get-git-root))
  (when (string-empty-p project-root)
    (error (format "ERORR: Not project root found!")))

  (setq project-build-script (looney-get-build-script project-root))
  (when (string-empty-p project-build-script)
    (error (format "ERORR: Have not found a suitable build script!")))

  (setq compile-options-windows
        `(,(executable-find "powershell")
          "-NoProfile"
          "-NoLogo"
          "-NonInteractive"
          "-ExecutionPolicy Bypass"
          "-File"
          ,project-build-script))

  ;; TODO: Test on GNU / Linux! Verified ONLY on Windows 11!
  (cond ((eq system-type 'windows-nt)
         (compile (mapconcat 'identity compile-options-windows " ")))
        ((eq system-type 'gnu/linux)
         (compile project-build-script))))

(global-set-key (kbd "<f9>") 'looney-build-current-project)

(provide 'build-project)

;;; build-project.el ends here
