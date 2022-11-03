;;; focus-mode.el --- Focus on the window at hand

;;; Commentary:
;; Excludes all distractions and focus on one's current task.  Clean frame.

;;; Code:
(defvar-local previous-mode-line-state nil)
(defvar focus-mode nil)

(define-minor-mode focus-mode
  "Minor mode to hide mode-line and use big fringes in current buffer."
  :init-value nil
  :global nil
  :variable focus-mode
  :group 'editing-basics
  (if focus-mode
      (progn
        (setq previous-mode-line-state mode-line-format)
        (setq-default mode-line-format nil)
        (display-line-numbers-mode -1)
        (set-fringe-mode
        (/ (- (frame-pixel-width)
              (* 100 (frame-char-width)))
           2)))
    (progn
      (setq-default mode-line-format previous-mode-line-state)
      (setq previous-mode-line-state nil)
      (display-line-numbers-mode +1)
      (set-fringe-style nil)))

  ;; Get rid of the indicators in the fringe
  (mapc (lambda(fb) (set-fringe-bitmap-face fb 'org-hide))
        fringe-bitmaps)

  (delete-other-windows)
  (toggle-frame-fullscreen)

  (force-mode-line-update)
  (redraw-display)

  (when (and (called-interactively-p 'interactive)
             focus-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Focus Mode enabled. "
             "Use M-x focus-mode to disable."))))

(provide 'focus-mode)
;;; focus-mode.el ends here
