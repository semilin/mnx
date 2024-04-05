(defpackage mnx
  (:use :cl :split-sequence)
  (:export #:new-derivation))
(in-package :mnx)

(defun pacman-query (query)
  (let ((lines (split-sequence #\newline (uiop:run-program (list "pacman" query) :output 'string :ignore-error-status t) :remove-empty-subseqs t)))
    (mapcar (lambda (line)
	      (car (split-sequence #\space line)))
	    lines)))

(defun pacman-get-installed ()
  (pacman-query "-Qe"))

(defun pacman-get-orphans ()
  (pacman-query "-Qdtq"))

(defun pacman-remove-packages (list)
  (uiop:run-program (cons "pacman" (cons "-R" list))
		    :input :interactive :output :interactive))

(defun manage-pacman ()
  (let* ((installed (pacman-get-installed))
	 (unneeded (remove-if (lambda (e) (member e *pacman-packages* :test 'equal)) installed)))
    (cond (unneeded
	   (format t "Removing ~a packages (~a)" (length unneeded) unneeded)
	   (pacman-remove-packages unneeded)))
    (let ((orphans (pacman-get-orphans)))
      (cond (orphans
	     (format t "Removing ~a orphaned packages (~a)" (length orphans) orphans)
	     (pacman-remove-packages orphans))))
    (uiop:run-program '("pacman" "-Syyu")
		      :input :interactive :output :interactive)))

(defun manage-emacs ())

(defun manage-packages ()
  (load "pacman.lisp")
  (manage-pacman))

(defun manage-configs ()
  (load "configs.lisp")
  (loop for (src . dst) in *config-paths*
	for tmp = (merge-pathnames (file-namestring src) "derivation/")
	do (progn (uiop:copy-file src tmp)
		  (sb-posix:chmod tmp 444)
		  (uiop:delete-file-if-exists dst)
		  (sb-posix:symlink (truename tmp) dst))))

(defun new-derivation ()
  (manage-packages)
  (manage-configs))
