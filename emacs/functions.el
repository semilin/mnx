(use-package plz
  :ensure t)

(defun semi/mnx ()
  (interactive)
  (async-shell-command "mnx"))

(defun semi/get-github-repos (user)
  (plz 'get (concat "https://api.github.com/users/" user "/repos" "?sort=pushed")
    :headers '(("Accept" . "application/vnd.github+json")
	             ("X-GitHub-Api-Version" . "2022-11-28"))
    
    :as #'json-read))

(defun semi/clone-own-github-ssh ()
  (interactive)
  (let ((repos (mapcar (lambda (repo)
                         (cdr (assq 'name repo)))
                       (semi/get-github-repos "semilin"))))
    (async-shell-command (concat "git clone git@github.com:semilin/"
                                 (completing-read "Choose repo to clone:" repos)
                                 ".git"))))
