(setq gc-cons-threshold 1000000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 500000)))

(setq package-enable-at-startup nil)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
