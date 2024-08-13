(let ((scale 1.0))
  (set-face-attribute 'default nil
		                  :font "Iosevka Nerd Font"
		                  :height (round (* scale 120)))

  (set-face-attribute 'fixed-pitch nil
		                  :font "Iosevka Nerd Font"
		                  :height (round (* scale 120)))

  (set-face-attribute 'variable-pitch nil
		                  :font "SourceSans3"
		                  :height (round (* scale 120))))

(defvar default-font "Iosevka Nerd Font")
(add-to-list 'default-frame-alist `(font . ,default-font))

(load-file "~/.config/emacs/elpaca.el")

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `italic', `WEIGHT'
      modus-themes-prompts '(italic bold)

      ;; The `modus-themes-completions' is an alist that reads two
      ;; keys: `matches', `selection'.  Each accepts a nil value (or
      ;; empty list) or a list of properties that can include any of
      ;; the following (for WEIGHT read further below):
      ;;
      ;; `matches'   :: `underline', `italic', `WEIGHT'
      ;; `selection' :: `underline', `italic', `WEIGHT'
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold italic text-also)))

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      ;; The `modus-themes-headings' is an alist: read the manual's
      ;; node about it or its doc string.  Basically, it supports
      ;; per-level configurations for the optional use of
      ;; `variable-pitch' typography, a height value as a multiple of
      ;; the base font size (e.g. 1.5), and a `WEIGHT'.
      modus-themes-headings
      '((1 . (variable-pitch 1.5))
        (2 . (1.3))
        (agenda-date . (1.3))
        (agenda-structure . (variable-pitch light 1.8))
        (t . (1.1))))

(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)

	(bg-mode-line-active "gray2")
	(bg-mode-line-inactive "gray1")

	(fringe 'unspecified)
	
	(fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)
        (bg-line-number-inactive bg-main)
        (bg-line-number-active bg-main)))

;; (use-package modus-themes
;;   :ensure t
;;   :init (load-theme 'modus-vivendi 't))

(use-package catppuccin-theme
  :ensure t
  :config (load-theme 'catppuccin 't))

(setq default-frame-alist '((cursor-color . "white")
			    (vertical-scroll-bars . nil)
			    (font . "Iosevka Nerd Font")))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(load-file "~/.config/emacs/config.el")
(load-file "~/.config/emacs/functions.el")

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)
