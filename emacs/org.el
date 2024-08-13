(use-package org
  :ensure t
  :config
  (setq org-capture-templates
        '(("t" "Inbox" entry (file+headline "~/org/capture/inbox.org" "Inbox")
           "* TODO %?\nSCHEDULED: %t\n%i")
          ("s" "Schedule" entry (file+headline "~/org/capture/inbox.org" "Inbox")
           "* %?\nSCHEDULED: %t\n%i")))

  (setq org-refile-targets (quote (("school.org" :maxlevel . 2)
                                   ("personal_projects.org" :maxlevel . 2)
                                   ("someday.org" :maxlevel . 2)
                                   ("wellbeing.org" :maxlevel . 2)
                                   ("events.org" :maxlevel . 1))))
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :hook ((org-mode . org-toggle-pretty-entities))
  :custom (org-log-done 'time))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode))

(use-package olivetti
  :ensure t)

(use-package org-variable-pitch
  :ensure t
  :hook ((org-mode . org-variable-pitch-minor-mode)))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;; (use-package org-superstar
;;   :hook (org-mode . org-superstar-mode))

;; (use-package org-recur
;;   :hook ((org-mode . org-recur-mode)
;;     (org-agenda-mode . org-recur-agenda-mode))
;;   :demand t
;;   :config
;;   (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

;;   (define-key org-recur-agenda-mode-map (kbd "D") 'org-recur-finish)
;;   (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)
;;   (setq org-recur-finish-done t
;;    org-recur-finish-archive t))

(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/home/semi/org/nodes")
  :config
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/nodes"))
  :config
  (require 'org-roam-protocol)
  :bind (("C-c r u" . org-roam-ui-open)))

(use-package org-node
  :ensure (:host github :repo "meedstrom/org-node")
  :hook ((org-mode . org-node-cache-mode)
         (org-mode . org-node-backlink-mode))
  :config
  (setq org-node-extra-id-dirs '("~/org/nodes")
        org-node-extra-id-dirs-exclude '(".sync-conflict-" "/archive/"))
  :bind (("C-c n f" . org-node-find)
         ("C-c n i" . org-node-insert-link)
         ("C-c n r" . org-node-random))
  :after org)

(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:auto-parent t))))
