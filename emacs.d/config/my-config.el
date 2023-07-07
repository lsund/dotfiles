;; package --- Summary
;;; Top level config file

;;; Commentary:

;;; Code:

(require-packages '(
                    ;; Evil mode adaptation plugins
                    evil-commentary
                    evil-surround

                    ;; Editing
                    drag-stuff
                    evil-cleverparens
                    wrap-region
                    web-mode
                    xquery-mode
                    multiple-cursors
                    company

                    ;; Search
                    evil-quickscope
                    highlight-symbol
                    swiper
                    counsel
                    rg

                    ;; Project navigation/management
                    flx-ido
                    ivy
                    magit
                    evil-magit
                    treemacs
                    treemacs-evil
                    treemacs-projectile
                    treemacs-magit
                    treemacs-icons-dired
                    smex
                    projectile
                    ggtags

                    ;; Visuals
                    badwolf-theme
                    blackboard-theme
                    linum-relative
                    powerline
                    airline-themes
                    origami
                    evil-goggles

                    ;; Other
                    flycheck
                    flymake
                    diminish
                    ))

(require 'my-functions)

(require 'uniquify)

;; Smex
(smex-initialize)

;; Drag stuff
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Highlight Symbol
(highlight-symbol-mode)

;; Evil Commentary (Code comments)
(evil-commentary-mode)

;; Evil Surround (emulating vim-surround)
(global-evil-surround-mode 1)

(ivy-mode)
(ido-mode 1)

(evil-goggles-mode 1)
(evil-goggles-use-diff-faces)

(setq evil-goggles-blocking-duration 0.100) ;; "before" overlays
(setq evil-goggles-async-duration 2.00) ;; "after" overlays

(origami-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings

;; Find files, switch buffers
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))

;; Auto fill
(set-fill-column 120)
(auto-fill-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(custom-set-variables '(indent-tabs-mode nil))

;; Unicode
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Ignore some buffers
(setq ido-ignore-buffers
      '("^ " "*Completions*" "*Shell Command Output*"
        "*Messages*" "Async Shell Command"))

;; Backup dir
(setq backup-by-copying t)

;; Backup to the below directory
(setq backup-directory-alist
      `(("." . , "/home/lsund/.emacs-backup-files/")))
(setq auto-save-file-name-transforms
      `((".*" , "/home/lsund/.emacs-backup-files/" t)))

;; Do not create .#fname lockfiles
(setq create-lockfiles nil)

;; Case sensitive search
(setq evil-ex-search-case 'sensitive)
(setq case-fold-search nil)

;; Do not ask about file save
(setq compilation-ask-about-save nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; For copy/paste from X11 to emacs
(setq  select-enable-clipboard t)
(setq select-enable-primary t)

;; Smooth instead of jumpy scrolling
(setq scroll-step 1 scroll-conservatively 10000)

;; Hide minor modes in mode line
(diminish 'autospace-mode)
(diminish 'whitespace-mode)
(diminish 'cider-mode)
(diminish 'projectile-mode)
(diminish 'flycheck-mode)
(diminish 'ivy-mode)
(diminish 'smartparens-mode)
(diminish 'company-mode)
(diminish 'drag-stuff-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'linum-relative-mode)
(diminish 'evil-cleverparens-mode)
(diminish 'eldoc-mode)
(diminish 'evil-commentary-mode)
(diminish 'cider-mode)
(diminish 'projectile-mode)
(diminish 'auto-fill-mode)
(diminish 'refill-mode)
(diminish 'whitespace-mode)
(diminish 'dante-mode)
(diminish 'haskell-indent-mode)
(diminish 'haskell-mode)
(diminish 'interactive-haskell-mode)
(diminish 'projectile-mode)
(diminish 'lsp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visuals

;; Status bar
(setq airline-display-directory "Full")
(setq airline-cursor-colors "Enabled")
(setq airline-eshell-colors "Disabled")
(setq airline-helm-colors "Disabled")
(load-theme 'airline-distinguished)

;; Color theme
(load-theme 'badwolf t)

;; Show matching parenthesis
(show-paren-mode 1)

;; Dont show welcome screen
(setq inhibit-startup-screen t)
(tool-bar-mode 0)

;; Highlight current line
(when window-system (global-hl-line-mode))

;; Highlight characters for f,t,F,T etc
(global-evil-quickscope-always-mode 1)

;; Do not open new windows
(setq pop-up-windows nil)

;; Line numbers
(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

;; Show current column
(setq column-number-mode t)

;; Do not show tool-bar with icons
(tool-bar-mode -1)

;; No menu bar
(menu-bar-mode 0)

;; No scroll bars
(scroll-bar-mode -1)

;; Delete trailing whitespace Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight excessive whitespace
(whitespace-mode 1)
(setq whitespace-line-column 120)
(setq whitespace-display-mappings
      '((newline-mark ?\n [?¬ ?\n])
        (space-mark ?\ [?. ])
        (space-mark ?\xA0 [?. ])
        (tab-mark ?\t [?\▸ ?\t])))

;; Font size and style
(set-face-attribute
 'default nil
 :family "Hack"
 ;; :family "Source Code Pro"
 :height (if (string= (car (split-string
                            (slurp "/etc/hostname") "\n" t))
                      "pedro")
             110
           140)
 :weight 'normal
 :width 'normal)

(setq uniquify-buffer-name-style 'reverse)
(setq inhibit-default-init t)
(setq-default frame-title-format "%b (%f)")

;; Do not wrap long lines by default
(set-default 'truncate-lines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling
;;
;; Use aspell, then hunspell
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']"
           nil
           ("-d" "en_US")
           nil
           utf-8)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin Configs

;; Git plugin
(evil-magit-init)
(setq magit-completing-read-function 'ivy-completing-read)
(setq evil-magit-state 'normal)

;; Autocompletion
(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends 'company-dabbrev-code)
(add-to-list 'company-backends 'company-yasnippet)
(add-to-list 'company-backends 'company-files)
(add-to-list 'company-backends 'company-elisp)

;; Project Navigation
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
(setq projectile-enable-caching nil)
(setq projectile-require-project-root nil)
(setq projectile-globally-ignored-directories
      (append
       '(".git"
         ".svn"
         "out"
         "repl"
         "target"
         "venv")
       projectile-globally-ignored-directories))
(setq projectile-globally-ignored-files
      (append
       '(".DS_Store"
         "*.gz"
         "*.pyc"
         "*.jar"
         "*.tar.gz"
         "*.tgz"
         "*.zip")
       projectile-globally-ignored-files))

(projectile-global-mode)

;; Recenter treemacs view after file and project change
(setq treemacs-recenter-after-file-follow 'always)
(setq treemacs-recenter-after-project-jump 'always)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require

(require 'my-keybindings)
(require 'my-clojure)
(require 'my-lisp)
(require 'my-elisp)
(require 'my-haskell)
(require 'my-org)
(require 'my-python)
(require 'my-tex)
(require 'my-markdown)
(require 'my-rust)
(require 'my-xml)
(require 'my-scala)
(require 'my-java)
(require 'my-javascript)

(provide 'my-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-config.el ends here
