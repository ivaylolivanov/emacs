;;; package --- Summary
;;; Commentary:
;;; This package aims to sync the git repository living at 'org-roam-directory'.
;;; If the repository has no commited changes, an auto-commit will be created.
;;; When there are no changes, it will do pull --rebase origin master

;;; Code:
(defun git-get-STDOUT (repository &rest args)
  "Execute a git command in REPOSITORY with ARGS parameters. Use process-lines to capture STDOUT."
  (apply #'process-lines "git" "-C" repository args))

(defun git-get-exit-code (repository &rest args)
  "Execute a git command in REPOSITORY with ARGS parameters. Use call-process to capture the exit code."
  (apply #'call-process "git" nil nil nil "-C" repository args))

(defun org-roam-git-autosync ()
  "Autosync the git repository pointed to by `org-roam-directory`."
  (interactive)
  (unless (boundp 'org-roam-directory)
    (user-error "Variable `org-roam-directory` is not defined"))

  (unless (vc-git-root org-roam-directory)
    (user-error "%s is not inside a git repository" org-roam-directory))

  ;; Move branch into this outer let
  (let* ((branch (car (git-get-STDOUT org-roam-directory
                                      "rev-parse" "--abbrev-ref" "HEAD")))
         (git-status
          (not (string-empty-p
                (mapconcat #'identity
                           (git-get-STDOUT org-roam-directory
                                           "status" "--porcelain")
                           "")))))


    ;; Commit if needed
    (when git-status
      (let ((commit-subject
             (format "AUTOSYNC %s"
                     (format-time-string "%Y-%m-%d %H:%M:%S"))))
        (git-get-exit-code org-roam-directory "add" "--update")
        (git-get-exit-code org-roam-directory "commit" "-s" "-m" commit-subject)))

    ;; Pull with --rebase
    (let ((exit-code
           (git-get-exit-code org-roam-directory
                              "pull" "--rebase" "origin" branch)))
      (if (= exit-code 0)
          (message "org-roam autosync succeeded")
        (message "org-roam autosync FAILED")))

    ;; Push only if we actually committed
    (when git-status
      (git-get-exit-code org-roam-directory "push" "origin" branch))))


(provide 'roam-sync)



(provide 'roam-sync)
;;; roam-sync.el ends here
