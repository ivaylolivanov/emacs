;;; org-settings --- Houses all org configurations

;;; Commentary:
;;=============
;;= Org setup =
;;=============
;; - Activate org
;; - Add time stamp when To-Do is closed
;; - Specify agenda directories
;; - Enable visual-line-mode within org-mode
;; - Configure org capture
;; - Org roam mode
;; - Publishing org-roam notes
;; - Keybindings



;;; Code:
;; - Activate org
(require 'org)

;; - Add time stamp when To-Do is closed
(setq org-log-done t)

;; - Specify agenda directories
(setq org-agenda-files (list org-roam-storage-dir))

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
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("g" "game design document" plain
      (file ,(file-name-concat org-roam-storage-dir
                               "/templates/game-design-document.org"))
      :target (file+head "game-designs/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "personal" plain ""
      :target (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
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
             :exclude ".*\\(templates\\|personal\\).*"
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
             :exclude ".*\\(templates\\|personal\\).*"
             )))

;; - Keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-roam-capture)

(provide 'org-settings)
;;; org-settings.el ends here
