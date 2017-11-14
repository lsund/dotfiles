;; package --- Summary
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
		    smex

		    ;; Visuals
		    badwolf-theme
		    linum-relative
		    powerline

		    ;; Other
		    flycheck
		    markdown-mode

		    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Minor Modes

;; Smex
(smex-initialize)

;; Winner
(winner-mode 1)

;; Show Parens
(show-paren-mode 1)

;; Drag stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Highlight Symbol
(highlight-symbol-mode)

;; Evil Commentary (Code comments)
(evil-commentary-mode)

;; Scroll bars
(scroll-bar-mode -1)

;; Evil Surround (emulating vim-surround)
(global-evil-surround-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(setq pop-up-windows nil)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Buffer switching
(setq ido-ignore-buffers
      '("^ " "*Completions*" "*Shell Command Output*"
	"*Messages*" "Async Shell Command"))

;; Unicode
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; Line numbers
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

;; Show current column
(setq column-number-mode t)

;; Show menu
(menu-bar-mode 1)

;; Do not show tool-bar with icons
(tool-bar-mode -1)


;; Backup dir
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq backup-by-copying t)

;; Do not create .#fname lockfiles

(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin Configs

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
;; Caching could greatly speed up file navigation on big projects
(setq projectile-enable-caching nil)
(setq projectile-require-project-root nil)
(setq projectile-globally-ignored-directories
      (append '(
        ".git"
        ".svn"
        "out"
        "repl"
        "target"
        "venv"
        )
          projectile-globally-ignored-directories))
(setq projectile-globally-ignored-files
      (append '(
        ".DS_Store"
        "*.gz"
        "*.pyc"
        "*.jar"
        "*.tar.gz"
        "*.tgz"
        "*.zip"
        )
          projectile-globally-ignored-files))
(projectile-global-mode)

;; Quick scope
(global-evil-quickscope-always-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search

;; Case sensitive
(setq evil-ex-search-case 'sensitive)
;; Smooth instead of jumpy scrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; Delete trailing whitespace on file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq compilation-ask-about-save nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; For copy/paste from X11 to emacs
(setq  select-enable-clipboard t)
(setq select-enable-primary t)

;; Font size
(set-face-attribute 'default nil
                    :family "Hack"
                    ;; :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Highlight current line
(when window-system
  (global-hl-line-mode))

;; Spell
(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
          )))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require

(require 'my-keybindings)
(require 'my-powerline)
(require 'my-clojure)
(require 'my-elisp)
(require 'my-haskell)
(require 'my-org)
(require 'my-python)
(require 'my-tex)
(require 'my-markdown)


(provide 'my-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-config.el ends here
