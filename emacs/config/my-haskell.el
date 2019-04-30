;;; Package --- Summary: My emacs haskell config

(require-packages '(
                    rainbow-delimiters
                    ;; shakespeare-mode
                    ;; haskell-mode
                    ;; intero
                    yaml-mode
                    dante
                    flycheck
                    ))

(require 'my-ghcid)

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
;; Haskell Mode

(add-hook 'dante-mode-hook
          '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                 '(warning . haskell-hlint))))

(add-hook 'haskell-mode-hook
          (lambda ()
            (interactive)

            (set-fill-column 80)
            (auto-fill-mode)

            (dante-mode)
            (flycheck-mode)

            (setq flymake-no-changes-timeout nil)
            (setq flymake-start-syntax-check-on-newline nil)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            ;; Save buffer after idle time
            ;; (auto-save-visited-mode 1)
            ;; (setq auto-save-visited-interval 1)

            ;; Leader map
            (defvar haskell-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" haskell-leader-map)


            ;; Keybindings
            (define-key haskell-leader-map "ii"
              'insert-haskell-comment-separator)

            (define-key haskell-leader-map "p" 'haskell-mode-stylish-buffer)

            ))

(provide 'my-haskell)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-haskell.el ends here
