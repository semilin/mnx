(setq use-package-always-ensure nil)

(setq backup-directory-alist
      `((".*" . "~/.config/emacs/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-version 6
      kept-old-version 2
      version-control t)
(setq create-lockfiles nil)

(setq-default indent-tabs-mode nil
              tab-width 2)

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
  (setq completion-cycle-threshold 3
	      tab-always-indent 'complete
	      sentence-end-double-space nil
	      use-dialog-box nil
	      global-auto-revert-non-file-buffers t)
  (savehist-mode 1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)
  (global-visual-line-mode 1)

  (setf use-default-font-for-symbols nil)
  (set-fontset-font t 'unicode "Noto Emoji" nil 'append)
  
  (set-face-background 'mode-line "gray13")
  
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
						                                           '("#a6e3a1" "black")
						                                         '("#cba6f7" "black"))))
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
  :bind (("C-o" . other-window))
  )

(use-package windmove
  :bind (("S-<right>" . windmove-right)
	       ("S-<left>" . windmove-left)
	       ("S-<up>" . windmove-up)
	       ("S-<down>" . windmove-down)))

(use-package tramp
  :config (setq tramp-default-method "ssh"))

(use-package age
  :ensure t
  :config
  (age-file-enable)
  :custom
  (age-program "rage"))

(use-package hy-mode
  :ensure t
  :mode "\\.hy\\'"
  :defer t)

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package c3-ts-mode
  :ensure (:repo "https://github.com/c3lang/c3-ts-mode")
  :config
  (setq c3-ts-mode-indent-offset 2)
  (setq treesit-font-lock-level 4))

(use-package wgsl-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :hook (go-mode . indent-tabs-mode))

(use-package markdown-mode
  :ensure t)

(use-package sly
  :ensure t) 

(use-package yaml
  :mode (("\\.yaml\\'" . yaml-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode))
  :defer t)

(use-package corfu
  :ensure t
  :custom (corfu-auto t)
  :config
  (global-corfu-mode)
  (setq corfu-auto-delay 0.0)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))
  
  (advice-add #'corfu-insert :after #'corfu-send-shell))

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
  (add-hook 'org-mode-hook
            (lambda ()
              (setq completion-at-point-functions '(#'cape-file))))
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
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package eglot
  :defer t
  :bind (("C-c C-l C-l" . eglot)
         ("C-c C-l C-r" . eglot-reconnect)
         ("C-c C-l C-h" . eglot-inlay-hints-mode)
         ("C-c C-l C-i" . eglot-code-action-organize-imports)
         ("C-c C-l C-p" . eglot-find-implementation)
         ("C-c C-l C-t" . eglot-find-typeDefinition)
         ("C-c C-l C-d" . eglot-find-declaration)
         ("C-c C-l C-a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions
                  (
                   :check (:command "clippy" :features "all")
                   :cargo (:features "all"))
                  ))))

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

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open-with
  :ensure (:host github :repo "FrostyX/dired-open-with"))

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
	       ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g ," . consult-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g m" . consult-mark)
         ("M-g o" . consult-outline)
         ("M-g e" . consult-compile-error)
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)))

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
  :init (setq avy-keys '(?s ?r ?n ?t ?d ?a ?i ?h ?m ?g ?l ?o ?b ?, ?j ?>))
  :bind ("C-," . avy-goto-word-1))

(use-package dashboard
  :ensure t
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  ;; vertically center content
  (setq dashboard-vertically-center-content t)
  :demand t
  )

;; (use-package telephone-line
;;   :config (telephone-line-mode +1))

;; (use-package paredit
;;   :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . paredit-mode))

(use-package puni
  :ensure t
  :hook ((prog-mode) . puni-mode)
  :bind (("C-<backspace>" . puni-backward-kill-word)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode hy-mode) . aggressive-indent-mode))

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

;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
	             (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (go "https://github.com/tree-sitter/tree-sitter-go")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (c3 "https://github.com/c3lang/tree-sitter-c3")))
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
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
	           (go-mode . go-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  )

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

;; (use-package treesitter-context
;;   :ensure (:host github :repo "zbelial/treesitter-context.el")
;;   :hook ((rust-ts-mode . treesitter-context-mode)
;;          (rust-ts-mode . treesitter-context-focus-mode)))

(use-package gptel
  :ensure t
  :bind (("C-c l" . gptel-menu))
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model "gemma2:latest"
	      gptel-backend (gptel-make-ollama "Ollama"           
			                  :host "localhost:11434"            
			                  :stream t                           
			                  :models '("gemma2:latest" "llama3:latest"))))

(use-package consult-omni
  :ensure (:host github :repo "armindarvish/consult-omni" :branch "main")
  :after consult
  :custom
  ;; General settings that apply to all sources
  (consult-omni-show-preview t) ;;; show previews
  (consult-omni-preview-key "C-o") ;;; set the preview key to C-o
  :config
  ;; Load Sources Core code
  (require 'consult-omni-sources)
  ;; Load Embark Actions
  (require 'consult-omni-embark)

  ;; Only load wikipedia source
  (setq consult-omni-sources-modules-to-load (list 'consult-omni-calc 'consult-omni-gptel 'consult-omni-fd 'consult-omni-wikipedia))
  (consult-omni-sources-load-modules)

  (setq consult-omni-multi-sources '("calc" "Org Agenda" "Wikipedia"))

  ;;; Set your shorthand favorite interactive command
  (setq consult-omni-default-interactive-command #'consult-omni-wikipedia))

;; (use-package elisa
;;   :init
;;   (setopt elisa-limit 5)
;;   ;; reranker increases answer quality significantly
;;   (setopt elisa-reranker-enabled t)
;;   ;; prompt rewriting may increase quality of answers
;;   ;; disable it if you want direct control over prompt
;;   (setopt elisa-prompt-rewriting-enabled t)
;;   (require 'llm-ollama)
;;   ;; gemma 2 works very good in my use cases
;;   ;; it also boasts strong multilingual capabilities
;;   (setopt elisa-chat-provider
;; 	        (make-llm-ollama
;; 	         :chat-model "gemma2:9b-instruct-q6_K"
;; 	         :embedding-model "chatfire/bge-m3:q8_0"
;; 	         ;; set context window to 8k
;; 	         :default-chat-non-standard-params '(("num_ctx" . 8192))))
;;   ;; this embedding model has stong multilingual capabilities
;;   (setopt elisa-embeddings-provider (make-llm-ollama :embedding-model "chatfire/bge-m3:q8_0"))
;;   :config
;;   ;; searxng works better than duckduckgo in my tests
;;   (setopt elisa-web-search-function 'elisa-search-searxng))

(use-package activities
  :ensure t
  :init
  (activities-mode)
  (activities-tabs-mode)
  :bind (("C-c v n" . activities-new)
	       ("C-c v d" . activities-define)
	       ("C-c v v" . activities-resume)
	       ("C-c v s" . activities-suspend)
	       ("C-c v k" . activities-kill)
	       ("C-c v /" . activities-switch)
	       ("C-c v b" . activities-switch-buffer)
	       ("C-c v f" . activities-switch-buffer)
	       ("C-c v g" . activities-revert)
	       ("C-c v l" . activities-list)))

(use-package zoom
  :ensure t
  :config
  (setq zoom-size '(0.55 . 0.55))
  (zoom-mode +1))

(use-package denote
  :ensure t
  :custom (denote-directory "~/doc/denote/"))

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
     '("(" . puni-backward-sexp)
     '(")" . puni-forward-sexp)
     '("^ s" . puni-splice)
     '("^ r" . puni-raise)
     '("^ b" . puni-barf-forward)
     '("^ l" . puni-slurp-forward)
     '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode))
