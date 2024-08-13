(use-package plz
  :ensure t)

(defun semi/mnx ()
  (interactive)
  (async-shell-command "mnx"))

(defun semi/github-get-user-repos (user)
  (plz 'get (concat "https://api.github.com/users/" user "/repos" "?sort=pushed")
    :headers '(("Accept" . "application/vnd.github+json")
	             ("X-GitHub-Api-Version" . "2022-11-28"))
    :as #'json-read))

(defun semi/github-get-user-starred (user)
  (plz 'get (concat "https://api.github.com/users" user "/starred" "?sort=created")
    :headers '(("Accept" . "application/vnd.github+json")
	             ("X-GitHub-Api-Version" . "2022-11-28"))
    :as #'json-read))

(defun github-parse-repos (response)
  (mapcar (lambda (repo)
            (cdr (assq 'name repo)))
          response))

(defun clone-repo (repo)
  (async-shell-command (concat "git clone " repo)))

(defun semi/clone-own-github-ssh ()
  (interactive)
  (let ((repos (github-parse-repos (semi/github-get-user-repos "semilin"))))
    (clone-repo (completing-read "Choose repo to clone:" repos))))
