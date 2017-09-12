(provide 'my-config)

(require-packages '(helm
		    cider
		    company
		    linum-relative
		    powerline
		    clojure-mode
		    aggressive-indent
		    tabbar
		    smartparens
		    evil-quickscope
		    evil-commentary
		    evil-cleverparens
		    drag-stuff

		    helm-ag
		    helm-projectile
		    projectile
		    magit
		    swiper-helm
		    rainbow-delimiters
		    badwolf-theme
		    highlight-symbol
		    markdown-mode
		    ))

;; Helm
(helm-mode 1)
(require 'helm-config)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)

;; Tabbar
(tabbar-mode 0)

;; Smartparens
(require 'smartparens-config)
(smartparens-mode 1)
(sp-pair "'" nil :actions :rem)

;; Aggressive indent
(global-aggressive-indent-mode 1)

;; Rainbow delimiters
(rainbow-delimiters-mode 1)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; Quick scope
(global-evil-quickscope-always-mode 1)

;; Powerline
(powerline-default-theme)

;; Misc
(linum-mode)
(linum-relative-global-mode)
(setq column-number-mode t)
(menu-bar-mode 1)
(tool-bar-mode -1)
(setq scroll-step 1
      scroll-conservatively 10000)

(setq-default show-trailing-whitespace t)
(setq compilation-ask-about-save nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)

;; Drag stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Highlight-symbol
(highlight-symbol-mode)

;; Commentary
(evil-commentary-mode)

;; Clevreparens

(evil-cleverparens-mode)

(require 'my-keybindings)
