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
;; Shakespeare mode

(add-hook 'shakespeare-hamlet-mode
	  (lambda ()
	    (setq tab-width 4)))

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

            (define-key evil-insert-state-map (kbd "TAB")
              'tab-to-tab-stop)

            (setq-default show-trailing-whitespace nil)
            (interactive-haskell-mode)))

(load-library "inf-haskell")
(setq haskell-program-name "stack ghci")

;; (add-hook 'haskell-mode-hook 'intero-mode)
;; (flycheck-add-next-checker 'intero '(warning . haskell-hlint))

(defun haskell-setup ()
  "Setup variables for editing Haskell files."
  (setq whitespace-line-column 70)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (number-sequence 4 80 4))
  (haskell-indentation-mode 0)
  (setq indent-line-function 'indent-relative))

(add-hook 'haskell-mode-hook 'haskell-setup)

(provide 'my-haskell)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-haskell.el ends here
