;;; org-settings --- Houses all org configurations

;;; Commentary:
;;=============
;;= Org setup =
;;=============
;; - Activate org
;; - Add time stamp when To-Do is closed
;; - Update org-todo-keywords
;; - Configure archiving TODO entries
;; - Specify agenda directories
;; - Enable visual-line-mode within org-mode
;; - Configure org capture
;; - Org roam mode
;; - Org Roam Quick Capture
;; - Make sure htmlize is installed
;; - Publishing org-roam notes
;; - Keybindings



;;; Code:
;; - Add time stamp when To-Do is closed
(setq org-log-done t)

;; - Specify agenda directories
(setq org-agenda-files
      (directory-files-recursively
       org-roam-storage-dir
       "\\.org$"))

;; - Update org-todo-keywords
(setq org-todo-keywords '((sequence "TODO" "PROG" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-todo)
        ("PROG" . "Goldenrod")
        ("DONE" . org-done)))

;; - Configure archiving TODO entries
(setq org-archive-location "%s::* Archived")

;; - Enable visual-line-mode within org-mode
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable 'org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode))

;; - Org roam mode
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :after org
  :custom
  (org-roam-directory (file-truename org-roam-storage-dir))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :target (file+head "%<%d%m%Y%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("g" "game design document" plain
      (file ,(file-name-concat org-roam-storage-dir
                               "/templates/game-design-document.org"))
      :target (file+head "game-designs/%<%d%m%Y%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "project management" plain ""
      :target (file+head "project-management/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :ProjectManagement:\n")
      :unnarrowed t)
     ("p" "personal" plain ""
      :target (file+head "personal/%<%d%m%Y%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("r" "cooking recipe" plain
      (file ,(file-name-concat org-roam-storage-dir
                               "/templates/cooking-recipe.org"))
      :target (file+head "%<%d%m%Y%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :target (file+head "%<%d-%m-%Y>.org"
                         "#+title: %<%d-%m-%Y>\n#+filetags: :Personal:\n"))))
  :bind (("C-c n f" . org-roam-node-find)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))
         (:map org-roam-dailies-map
               ("Y" . org-roam-dailies-capture-yesterday)
               ("T" . org-roam-dailies-capture-tomorrow)))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))

;; - Org Roam Quick Capture
(defun org-roam-quick-capture ()
  "This function populates file with quick capture notes."
  (interactive)
  (org-roam-capture-
   :node (org-roam-node-create)
   :templates
   '(("q" "quick-capture" plain "* %<%H:%M:%S-%d-%m-%Y>\n%?"
      :if-new (file+head "quick-capture.org"
                         "#+title: Quick capture\n#+filetags: :Personal:QuickCapture:\n")))))
(global-set-key (kbd "C-c n q") #'org-roam-quick-capture)


;; - Make sure htmlize is installed
;; It is required by the publish feature.
(use-package htmlize
  :ensure t)

;; - Publishing org-roam notes
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil)
(setq org-publish-project-alist
      (list
       (list "roam-notes:main"
             :recursive t
             :base-directory org-roam-storage-dir
             :publishing-function 'org-html-publish-to-html
             :publishing-directory (file-name-concat org-roam-storage-dir  "publish")
             :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"./style/style.css\" />"
             :exclude ".*\\(templates\\|daily\\|quick-capture\\|game-designs\\|personal\\|project-management\\).*"
             :with-author nil
             :with-creator nil
             :with-toc nil           ;; Do not include a table of contents
             :auto-sitemap t
             :sitemap-filename "index.org"
             :sitemap-title "Ivaylo's public learning"
             :section-numbers nil
             :time-stamp-file nil)
       (list "roam-notes:static"
             :base-directory org-roam-storage-dir
             :publishing-directory (file-name-concat org-roam-storage-dir "publish")
             :recursive t
             :base-extension "css\\|png\\|jpg\\|gif"
             :publishing-function 'org-publish-attachment
             :exclude ".*\\(templates\\|daily\\|quick-capture\\|game-designs\\|personal\\|project-management\\).*"
             )))

;; - Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-roam-capture)

(provide 'org-settings)
;;; org-settings.el ends here
