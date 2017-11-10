;;; Package --- Summary: My emacs haskell config

(require-packages '(

		    rainbow-delimiters

		    shakespeare-mode
		    haskell-mode
		    ;; intero

		    ))

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor Modes

(rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun insert-haskell-comment-separator ()
  "Insert a comment separator."
  (interactive)
  (insert
   (format "%s%s"
	   "--------------------------------------------------------------------"
	   "------------"))
  (open-line 1)
  (evil-next-line)
  (insert "-- ")
  (evil-insert 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Interactive


(add-hook 'haskell-interactive-mode-hook
          (lambda ()
            (interactive)
            (evil-define-key
              'insert
              haskell-interactive-mode-map
              (kbd "C-p")
              'haskell-interactive-mode-history-previous)
            (evil-define-key
              'insert
              haskell-interactive-mode-map
              (kbd "C-n")
              'haskell-interactive-mode-history-next)
            (evil-define-key
              'insert
              haskell-interactive-mode-map
              (kbd "C-u")
              'haskell-interactive-mode-kill-whole-line)
            (evil-define-key
              'insert
              haskell-interactive-mode-map
              (kbd "C-a")
              'haskell-interactive-mode-beginning)
            (evil-define-key
              'insert
              haskell-interactive-mode-map
              (kbd "C-e")
              (lambda ()
                (interactive)
                (evil-end-of-line)
                (forward-char)))
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cabal mode



(add-hook 'haskell-cabal-mode-hook
          (lambda ()
            (interactive)

            ;; Auto fill
            (set-fill-column 80)
            (auto-fill-mode)

            ;; Manual tabs
            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq indent-line-function 'tab-to-tab-stop)
            (setq tab-stop-list (number-sequence 4 120 4))

            (setq-default show-trailing-whitespace nil)

            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shakespeare mode


(add-hook 'shakespeare-hamlet-mode-hook
          (lambda ()
            (interactive)

            ;; Auto fill
            (set-fill-column 80)
            (auto-fill-mode)

            ;; Manual tabs
            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq indent-line-function 'tab-to-tab-stop)
            (setq tab-stop-list (number-sequence 4 120 4))

            (setq-default show-trailing-whitespace nil)

            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Mode


(add-hook 'haskell-mode-hook
          (lambda ()
            (interactive)

            ;; Leader map
            (defvar haskell-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" haskell-leader-map)

            ;; Auto fill
            (set-fill-column 80)
            (auto-fill-mode)

            ;; Keybindings
            (define-key haskell-leader-map "ii"
              'insert-haskell-comment-separator)

            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq indent-line-function 'tab-to-tab-stop)
            (setq tab-stop-list (number-sequence 4 120 4))

            (setq-default show-trailing-whitespace nil)
            (interactive-haskell-mode)))

(load-library "inf-haskell")
(setq haskell-program-name "stack ghci")

;; (add-hook 'haskell-mode-hook 'intero-mode)
;; (flycheck-add-next-checker 'intero '(warning . haskell-hlint))

(provide 'my-haskell)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-haskell.el ends here
