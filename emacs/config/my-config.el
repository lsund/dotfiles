;;; package --- Summary
;;; Top level config file
 
;;; Commentary:

;;; Code:

(require-packages '(

		    ;; Evil mode adaptation plugins
		    evil-commentary

		    ;; Editing
		    drag-stuff
		    evil-cleverparens

		    ;; Search
		    evil-quickscope
		    highlight-symbol

		    ;; Project navigation/management
		    projectile
		    swiper
		    counsel
		    counsel-projectile
		    magit
		    evil-magit
		    neotree

		    ;; Visuals
		    badwolf-theme
		    org-bullets
		    linum-relative
		    powerline

		    ;; Other
		    org

		    ;; Python
		    flycheck

		    ;; Haskell
		    haskell-mode
		    intero

		    ))

;; Flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save new-line))

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
	    ((org-bullets-mode t))))

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

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
; (helm-projectile-on)
(setq projectile-enable-caching t)

;; Quick scope
(global-evil-quickscope-always-mode 1)

;; Powerline
(defun powerline-my-theme ()
  "My custom powerline theme."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" mode-line 'l)
				     ;; Buffer size
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size mode-line 'l))
				     ;; Module info
				     (when powerline-display-mule-info
				       (powerline-raw mode-line-mule-info mode-line 'l))
				     (powerline-buffer-id mode-line-buffer-id 'l)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%4l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" mode-line 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

(powerline-my-theme)

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

;; Haskell

;; Show trailing whitespace
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (setq-default show-trailing-whitespace t)
	    (interactive-haskell-mode)))
;; (add-hook 'haskell-mode-hook 'intero-mode)
;; (flycheck-add-next-checker 'intero '(warning . haskell-hlint))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(require 'my-clojure)
(require 'my-elisp)
(require 'my-keybindings)
(provide 'my-config)
;;; my-config.el ends here

