(use-package org
  :config
  (setq org-capture-templates
	'(("t" "Inbox" entry (file+headline "~/org/capture/inbox.org" "Inbox")
	   "* TODO %?\nSCHEDULED: %t\n%i")
	  ("s" "Schedule" entry (file+headline "~/org/capture/inbox.org" "Inbox")
	   ("* %?\nSCHEDULED: %t\n%i"))))

  (setq org-refile-targets (quote (("school.org" :maxlevel . 2)
				   ("personal_projects.org" :maxlevel . 2)
				   ("someday.org" :maxlevel . 2)
				   ("wellbeing.org" :maxlevel . 2))))
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :hook (org-mode . auto-fill-mode))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

;; (use-package org-superstar
;;   :hook (org-mode . org-superstar-mode))

;; (use-package org-recur
;;   :hook ((org-mode . org-recur-mode)
;; 	 (org-agenda-mode . org-recur-agenda-mode))
;;   :demand t
;;   :config
;;   (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

;;   (define-key org-recur-agenda-mode-map (kbd "D") 'org-recur-finish)
;;   (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)
;;   (setq org-recur-finish-done t
;; 	org-recur-finish-archive t))

(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :config
  (require 'org-roam-protocol)
  :bind (("C-c r u" . org-roam-ui-open)
	 ("C-c r u" . org-roam-buffer-toggle)
	 ("C-c r f" . org-roam-node-find)
	 ("C-c r i" . org-roam-node-insert)
	 ("C-c r r" . org-roam-node-random)))

(use-package org-roam-ui
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :config
  (require 'org-roam-protocol)
  :bind (("C-c r u" . org-roam-ui-open)))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
	'((:auto-group t))))
