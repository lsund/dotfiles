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

                    ;; Project navigation/management
                    flx-ido
                    ivy
                    magit
                    evil-magit
                    neotree
                    smex

                    ;; Visuals
                    badwolf-theme
                    blackboard-theme
                    color-theme-solarized
                    linum-relative
                    powerline
                    airline-themes

                    ;; Other
                    flycheck
                    diminish
                    ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visuals

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search

;; Case sensitive
(setq evil-ex-search-case 'sensitive)

;; Do not ask about file save
(setq compilation-ask-about-save nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; For copy/paste from X11 to emacs
(setq  select-enable-clipboard t)
(setq select-enable-primary t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look & Feel

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(whitespace-mode 1)
(setq whitespace-line-column 120)
(setq whitespace-display-mappings
      '((newline-mark ?\n [?¬ ?\n])
        (space-mark ?\ [?. ])
        (space-mark ?\xA0 [?. ])
        (tab-mark ?\t [?\▸ ?\t])))


;; Set font style and size depending on host
(defun slurp (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
       (point-min)
       (point-max))))

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

;; Smooth instead of jumpy scrolling
(setq scroll-step 1 scroll-conservatively 10000)

(setq uniquify-buffer-name-style 'reverse)
(setq inhibit-default-init t)
(setq-default frame-title-format "%b (%f)")

(define-key evil-normal-state-map (kbd "C-+") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "C-0")
  (lambda () (interactive) (text-scale-increase 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling
;;
;; try aspell at first if hunspell does NOT exist, use hunspell
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

;; New Leader map
(defvar my-leader-map (make-sparse-keymap))
(define-key evil-normal-state-map "\\" my-leader-map)
;; Resets
(define-key my-leader-map "n" nil)
(define-key my-leader-map "N" nil)

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))

(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))


;; Magit magit

(define-key evil-normal-state-map (kbd "gs") 'magit-status)

;; Align

(define-key evil-visual-state-map (kbd "M-a") 'align-regexp)

(setq case-fold-search nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Require

(require 'my-bufferman)
(require 'my-editing)
(require 'my-powerline)
(require 'my-clojure)
(require 'my-xml)
(require 'my-elisp)
(require 'my-haskell)
(require 'my-org)
(require 'my-python)
(require 'my-tex)
(require 'my-markdown)
(require 'my-projectile)
(require 'my-ivy)
(require 'my-neotree)
(require 'my-rust)

(provide 'my-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-config.el ends here
