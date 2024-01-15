;;; package --- Summary
;;; Commentary:
;;; This package tries to get the current git root, then depending on the OS,
;;; either find 'build.ps' or 'build.sh'.  If such script is found, executes it.

;;; Code:

(defun looney-get-git-root ()
  "Get the current Git root directory."
  (setq current-directory (file-name-directory buffer-file-name))
  (if (fboundp 'vc-git-root)
      (expand-file-name (vc-git-root current-directory))
    (cond ((eq system-type 'gnu/linux)
           (shell-command-to-string "git rev-parse --show-toplevel"))
          ((eq system-type 'windows-nt)
           (message "Not implemented for windows. Stay tuned...")))))

(defun looney-get-build-script (directory)
  "Try to find depending on the current OS build script in DIRECTORY."
  (setq build-script-name
        (cond ((eq system-type 'windows-nt) "build.ps")
              ((eq system-type 'gnu/linux)  "build.sh")))
  (if (directory-name-p directory)
      (let ((build-script-file (directory-files-recursively directory build-script-name)))
        (if build-script-file
            (progn
              (setq build-script-abs-path (car build-script-file))
              (if (and
                   (file-readable-p build-script-abs-path)
                   (file-executable-p build-script-abs-path))
                  (message build-script-abs-path)
                (message (format "ERROR: Build script %s cannot be executed!" build-script-abs-path))))
          (messasge (format "No build script found in %s!" directory))))
    (message (format "ERROR: %s does not exist" directory))))

(defun looney-build-current-project ()
  "Try to execute the build script for the current project."
  (interactive)
  (setq build-status-buffer-name "*Build-Status*")
  (setq project-root (looney-get-git-root))
  (setq build-status-buffer
        (if (get-buffer build-status-buffer-name)
            (get-buffer build-status-buffer-name)
          (generate-new-buffer build-status-buffer-name)))
  (if project-root
      (progn
       (setq project-build-script (looney-get-build-script project-root))
       (if project-build-script
           (let ((build-status (shell-command-to-string project-build-script)))
             (with-current-buffer build-status-buffer
               (erase-buffer)
               (insert build-status)
               (split-window-horizontally))
             (switch-to-buffer-other-window build-status-buffer))
         (message (format "Failed to build project at %s." project-root))))
    (message "No valid project found!")))

(global-set-key (kbd "<f9>") 'looney-build-current-project)

(provide 'build-project)

;;; build-project.el ends here
