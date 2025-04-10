;;; package --- Summary

;;; Commentary:
;; - Setup melpa in separate file to keep the configurations clean



;;; Code:
;;========================
;;= MELPA Configurations =
;;========================
(require 'package)
(setq package-install-upgrade-built-in t)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Workaround for unavailable packages
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;========================

(provide 'melpa)
;;; melpa.el ends here
