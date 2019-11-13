;;; Package --- Summary: My emacs rust config

(require-packages '(
                    evil
                    flycheck
                    flycheck-rust
                    rust-mode
                    ))

;;; Commentary:

;;; Code:

(defun insert-rust-comment-separator ()
  "Insert a comment separator."
  (interactive)
  (insert
   (format "%s%s"
	   "////////////////////////////////////////////////////////////////////"
	   "////////////"))
  (open-line 1)
  (evil-next-line)
  (insert "// ")
  (evil-insert 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook
          (lambda ()
            (interactive)

            (flycheck-rust-setup)

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Keybindings

            (defvar rust-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" rust-leader-map)

            ;; Insert separator
            (define-key rust-leader-map "ii" 'insert-rust-comment-separator)

            ;; Format buffer
            (define-key rust-leader-map "bd" 'rust-format-buffer)


            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Configuration

            ;; rainbow parenthesis
            ;; (rainbow-delimiters-mode)

            (set-fill-column 80)
            (auto-fill-mode)

            ;; Spell checking
            (flycheck-mode)
            ;; (dante-mode)
            (setq flymake-no-changes-timeout nil)
            (setq flymake-start-syntax-check-on-newline nil)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))))

(provide 'my-rust)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-rust.el ends here
