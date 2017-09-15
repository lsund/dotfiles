(provide 'my-config)

(require-packages '(

		    ;; Clojure
		    clojure-mode
		    cider

		    ;; Evil mode adaptation plugins
		    evil-commentary

		    ;; Editing
		    drag-stuff
		    paredit
		    paxedit
		    evil-cleverparens
		    smartparens

		    ;; Search
		    evil-quickscope
		    highlight-symbol

		    ;; Project navigation/management
		    projectile
		    swiper
		    magit
		    evil-magit
		    neotree
		    tabbar

		    ;; Visuals
		    rainbow-delimiters
		    badwolf-theme
		    org-bullets
		    linum-relative
		    powerline

		    ;; TODO
		    org
		    counsel
		    company

		    ))

;; Unicode
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Org mode

(setq org-time-stamp-formats
      '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))

(setq org-support-shift-select t)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DONE")))

(setq org-log-done t)

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis " ↴")

;; Ivy

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Magit

(evil-magit-init)
(setq magit-completing-read-function 'ivy-completing-read)
(setq evil-magit-state 'normal)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Auto complete

;; (ac-config-default)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
; (helm-projectile-on)
(setq projectile-enable-caching t)

;; Tabbar
(tabbar-mode 0)

;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(sp-pair "'" nil :actions :rem)

;; Cleverparens
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)

;; Aggressive indent
(global-aggressive-indent-mode -1)

;; Rainbow delimiters
(rainbow-delimiters-mode 1)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;; Quick scope
(global-evil-quickscope-always-mode 1)

;; Powerline
(powerline-default-theme)

;; Line numbers
(linum-mode)
(linum-relative-global-mode)

;; Show current column
(setq column-number-mode t)

;; Show menu
(menu-bar-mode 1)

;; Do not show tool-bar with icons
(tool-bar-mode -1)

;; Smooth instead of jumpy scrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)
;; Delete trailing whitespace on file save
(add-hook 'before-save-hood 'delete-trailing-whitespace)
(setq compilation-ask-about-save nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; For copy/paste from X11 to emacs
(setq  x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; Font size
(set-face-attribute 'default nil :height 140)


;; Highlight current line
(when window-system
  (global-hl-line-mode))

;; Show matching paranthesis
(show-paren-mode 1)

;; Drag stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Highlight-symbol
(highlight-symbol-mode)

;; Commentary
(evil-commentary-mode)

;; Scroll bars
(scroll-bar-mode -1)

(require 'my-keybindings)
