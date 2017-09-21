;;; package --- Summary
;;; Top level config file

;;; Commentary:

;;; Code:

(require-packages '(

		    company
		    ;; Evil mode adaptation plugins
		    evil-commentary
		    evil-surround

		    ;; Editing
		    drag-stuff
		    evil-cleverparens

		    ;; Search
		    evil-quickscope
		    highlight-symbol

		    ;; Project navigation/management
		    projectile
		    flx-ido
		    swiper
		    counsel
		    counsel-projectile
		    magit
		    evil-magit
		    neotree

		    ;; Visuals
		    badwolf-theme
		    linum-relative
		    powerline

		    ;; Other
		    flycheck

		    ))

;; Buffer switching
(setq ido-ignore-buffers
      '("^ " "*Completions*" "*Shell Command Output*"
	"*Messages*" "Async Shell Command"))

;; Flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save new-line))

;; Unicode
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Ivy

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))


;; Magit

(evil-magit-init)
(setq magit-completing-read-function 'ivy-completing-read)
(setq evil-magit-state 'normal)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends 'company-dabbrev-code)
(add-to-list 'company-backends 'company-yasnippet)
(add-to-list 'company-backends 'company-files)
(add-to-list 'company-backends 'company-elisp)

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
; (helm-projectile-on)
(setq projectile-enable-caching t)

;; Quick scope
(global-evil-quickscope-always-mode 1)

;; Line numbers
(linum-mode)
(linum-relative-global-mode)

;; Show current column
(setq column-number-mode t)

;; Show menu
(menu-bar-mode 1)

;; Do not show tool-bar with icons
(tool-bar-mode -1)

;; Search
;; Case sensitive
(setq evil-ex-search-case 'sensitive)
;; Smooth instead of jumpy scrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; Delete trailing whitespace on file save
(add-hook 'before-save-hood 'delete-trailing-whitespace)
(setq compilation-ask-about-save nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; For copy/paste from X11 to emacs
(setq  select-enable-clipboard t)
(setq select-enable-primary t)

;; Font size
(set-face-attribute 'default nil
                    :family "Hack"
                    ;; :family "Source Code Pro"
                    :height 140
                    :weight 'normal
                    :width 'normal)


;; Windows
(winner-mode 1)

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

;; Evil Surround

(global-evil-surround-mode 1)

(require 'my-powerline)
(require 'my-clojure)
(require 'my-elisp)
(require 'my-haskell)
(require 'my-keybindings)
(require 'my-org)

(provide 'my-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-config.el ends here

