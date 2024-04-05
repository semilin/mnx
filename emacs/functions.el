(defvar guix-custom-dir "/home/semi/code/guix-config/custom/")
(defvar guix-home-scm "/home/semi/code/guix-config/home.scm")

(require 'plz)

(defun semi/guix-home-reconfigure ()
  (interactive)
  (async-shell-command (concat "guix home reconfigure -L " guix-custom-dir " " guix-home-scm) "Guix Home Reconfigure"))

(defun semi/get-github-repos (user)
  (plz 'get (concat "https://api.github.com/users/" user "repos")
    :headers '(("Accept" . "application/vnd.github+json")
	       ("X-GitHub-Api-Version: 2022-11-28"))
    :as #'json-read))

(defun semi/clone-own-github-ssh (name)
  (interactive "sRepo name: \n")
  (async-shell-command (concat "git clone git@github.com:semilin/" name ".git")))
