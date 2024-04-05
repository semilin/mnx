(setq use-package-always-ensure nil)

(setq backup-directory-alist `(("." . "~/.config/emacs/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-version 6
      kept-old-version 2
      version-control t)

(use-package proced
  :ensure nil
  :commands proced
  :bind (("C-M-p" . proced))
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args comm))))


(add-hook 'prog-mode-hook (lambda ()
			    (setq display-line-numbers-type 'relative)
			    (display-line-numbers-mode)
			    (electric-pair-mode)))

:::cl
;; fontify doc strings in correct face
;; lisp-mode already fontifies 'defun*' correctly
(put 'defvar* 'doc-string-elt 3)
(put 'defparameter* 'doc-string-elt 3)
(put 'lambda* 'doc-string-elt 2)

(defvar *lisp-special-forms*
  (regexp-opt '("defvar*"
		"defconstant*"
		"defparameter*"
		"defgeneric*"
		"defmethod*"
		"defun*"
		"lambda*"
		"flet*"
		"labels*")
	      'words))

(font-lock-add-keywords 'lisp-mode
			`((,*lisp-special-forms* . font-lock-keyword-face)))

(use-package all-the-icons
  :ensure t)

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)

  (set-face-background 'mode-line "gray10")
  (set-face-background 'mode-line-highlight "gray19")

  ;; taken from https://www.reddit.com/r/emacs/comments/1333621/wrote_a_custom_modeline_with_some_help_from/
  (defun ntf/mode-line-format (left right)
    "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
    (let ((available-width (- (window-width) (length left) 1)))
      (format (format "%%s %%%ds " available-width) left right)))

  (setq-default mode-line-format
		'((:eval (ntf/mode-line-format
			  ;; left
			  (format-mode-line
			   (quote
			    ((:eval (let* ((ind (substring (meow-indicator) 1 2))
					   (colors (if (string= ind "I")
						       '("medium spring green" "black")
						     '("purple" "white"))))
				      (propertize (concat " " ind " ") 'face `(:background ,(car colors) :foreground ,(cadr colors)))))
			     " %* "
			     (:eval (propertize "%b " 'face 'bold))
			     "%m "
			     "%l:%c "
			     mode-line-percent-position
			     "%%")))
			  ;; right
			  (format-mode-line (quote ((:eval (when (org-clock-is-active)
							     (org-clock-get-clock-string)))
						    (vc-mode vc-mode))))
			  ))))

  (setq display-time-string-forms
	'((propertize (format-time-string "%H:%M") 'face 'bold)))
  (display-time-mode)
  )

(use-package tramp
  :config (setq tramp-default-method "ssh"))

(use-package hy-mode
  :ensure t
  :mode "\\.hy\\'"
  :defer t)

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :defer t)

(use-package go-mode
  :ensure t)

(use-package sly
  :ensure t) 

(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode)
  :defer t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  :defer t)

(use-package corfu
  :ensure t
  :custom (corfu-auto t)
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-elisp-symbol)
         ("M-p e" . cape-elisp-block)
         ("M-p a" . cape-abbrev)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p j" . cape-emoji)
         ("M-p \\" . cape-tex)
         ("M-p _" . cape-tex)
         ("M-p ^" . cape-tex)
         ("M-p &" . cape-sgml)
         ("M-p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete)
	 ("M-*" . tempel-insert))

  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode . tempel-setup-capf)
	 (text-mode . tempel-setup-capf)))

(use-package eglot
  :defer t)

(use-package ligature
  :ensure t
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures '(c-mode rust-mode rust-ts-mode)
			  '("->" "==" "!=" "<=" ">=" "=>" "/*" "*/"))
  (ligature-set-ligatures '(lisp-mode sly-mrepl-mode sly-mode)
			  '("->" "->>" "<>" "<=" ">=" "/=" ";;")))

(use-package vertico
  :ensure t
  :config (vertico-mode +1))

(use-package vertico-posframe
  :ensure t
  :init
  (setq vertico-posframe-parameters
	'((left-fringe . 8)
          (right-fringe . 8)))
  :config (vertico-posframe-mode +1))

(use-package orderless
  :ensure t
  :init (setq completion-styles '(orderless basic)
	      completion-category-defaults nil
	      completion-category-overrides ''(file (styles partial-completion))))

(use-package marginalia
  :ensure t
  :config (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("C-x p b" . consult-project-buffer)
	 ("M-y" . consult-yank-pop)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package expand-region
  :ensure t
  :bind ("C-;" . er/expand-region))

(use-package avy
  :ensure t
  :init (setq avy-keys '(?s ?r ?n ?t ?d ?a ?i ?h ?m ?g))
  :bind ("C-," . avy-goto-word-1))

(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  :demand t)

;; (use-package telephone-line
;;   :config (telephone-line-mode +1))

;; (use-package paredit
;;   :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . paredit-mode))

(use-package puni
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . puni-mode)
  :bind (("M-r" . puni-raise)
	 ("M-s" . puni-splice)
	 ("C-<backspace>" . puni-backward-kill-word)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . rainbow-delimiters-mode))

;; (use-package aggressive-indent
;;   :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . aggressive-indent-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit)))

(use-package projectile
  :ensure t)

(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode +1))

;; (use-package scroll-on-jump
;;   :config
;;   (global-set-key (kbd "C-v") (scroll-on-jump-with-scroll-interactive 'scroll-up-command))
;;   (global-set-key (kbd "M-v") (scroll-on-jump-with-scroll-interactive 'scroll-down-command)))

;; (use-package circe
;;   :config
;;   ;; This loads my libera.chat authentication information, which isn't
;;   ;; stored here.
;;   (load-file "/home/semi/.config/emacs/circe.el"))

(load-file "/home/semi/.config/emacs/org.el")
;; (load-file "/home/semi/.config/emacs/elfeed.el")

;; `M-x combobulate' (or `C-c o o') to start using Combobulate
;; pretty sure this doesn't work at all :grofl:
(use-package treesit
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
	       (rust "https://github.com/tree-sitter/tree-sitter-rust")
	       (c "https://github.com/tree-sitter/tree-sitter-c")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
		     (c-mode . c-ts-mode)
		     (rust-mode . rust-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/.config/emacs/combobulate/")))

(use-package meow
  :ensure t
  :init
  (defconst meow-cheatsheet-layout-semimak-jq
    '((<TLDE> "`" "~")
      (<AE01> "1" "!")
      (<AE02> "2" "@")
      (<AE03> "3" "#")
      (<AE04> "4" "$")
      (<AE05> "5" "%")
      (<AE06> "6" "^")
      (<AE07> "7" "&")
      (<AE08> "8" "*")
      (<AE09> "9" "(")
      (<AE10> "0" ")")
      (<AE11> "-" "_")
      (<AE12> "=" "+")
      (<AD01> "f" "F")
      (<AD02> "l" "L")
      (<AD03> "h" "H")
      (<AD04> "v" "V")
      (<AD05> "z" "Z")
      (<AD06> "'" "\"")
      (<AD07> "w" "W")
      (<AD08> "u" "U")
      (<AD09> "o" "O")
      (<AD10> "y" "Y")
      (<AD11> "[" "{")
      (<AD12> "]" "}")
      (<BKSL> "\\" "|")
      (<AC01> "s" "S")
      (<AC02> "r" "R")
      (<AC03> "n" "N")
      (<AC04> "t" "T")
      (<AC05> "k" "K")
      (<AC06> "c" "C")
      (<AC07> "d" "D")
      (<AC08> "e" "E")
      (<AC09> "a" "A")
      (<AC10> "i" "I")
      (<AC11> ";" ":")
      (<AB01> "j" "J")
      (<AB02> "b" "B")
      (<AB03> "m" "M")
      (<AB04> "q" "Q")
      (<AB05> "x" "X")
      (<AB06> "p" "P")
      (<AB07> "g" "G")
      (<AB08> "," "<")
      (<AB09> "." ">")
      (<AB10> "/" "?")
      (<LSGT> "-" "_")))
  
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-semimak-jq)
    (meow-motion-overwrite-define-key
     ;; Use e to move up, n to move down.
     ;; Since special modes usually use n to move down, we only overwrite e here.
     ;; '("e" . meow-prev)
     ;; '("d" . meow-next)
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("?" . meow-cheatsheet)
     ;; To execute the originally e in MOTION state, use SPC e.
     '("e" . "H-e")
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("/" . meow-visit)
     '("a" . meow-right)
     '("A" . meow-right-expand)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-left)
     '("C" . meow-left-expand)
     '("d" . meow-next)
     '("D" . meow-next-expand)
     '("e" . meow-prev)
     '("E" . meow-prev-expand)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     ;;'("h" . meow-change)
     ;;'("H" . meow-next-expand)
     '("i" . meow-append)
     '("I" . meow-open-below)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("K" . meow-delete)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-change)
     ;;'("N" . meow-open-above)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-search)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("y" . meow-save)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode))
