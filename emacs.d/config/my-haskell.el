;;; Package --- Summary: My emacs haskell config

(require-packages '(
                    rainbow-delimiters
                    ;; shakespeare-mode
                    haskell-mode
                    ;; intero
                    yaml-mode
                    flycheck
                    hindent
                    ))

(require 'haskell-interactive-mode)
(require 'haskell-process)

;;; Commentary:

;;; Code:

;; rainbow parenthesis
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
;; Haskell Repl

(eval-after-load "haskell-interactive-mode"
  '(progn
     ;; End of prompt
     (evil-define-key 'insert haskell-interactive-mode-map
       (kbd "C-e") 'end-of-line)
     ;; Beginning of prompt
     (evil-define-key 'insert haskell-interactive-mode-map
       (kbd "C-a") (lambda ()
                     (interactive)
                     (beginning-of-line)
                     (evil-forward-WORD-begin)))
     ;; Delete prompt
     (evil-define-key 'insert haskell-interactive-mode-map
       (kbd "C-u") (lambda ()
                     (interactive)
                     (beginning-of-line)
                     (evil-forward-WORD-begin)
                     (kill-line)))
     ;; Step back in history
     (evil-define-key 'insert haskell-interactive-mode-map
       (kbd "C-p") 'haskell-interactive-mode-history-previous)
     ;; Step forward in history
     (evil-define-key 'insert haskell-interactive-mode-map
       (kbd "C-n") 'haskell-interactive-mode-history-next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Editing

;; (with-eval-after-load 'dante-mode
;;   (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            (interactive)

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Keybindings

            (defvar haskell-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" haskell-leader-map)

            ;; Insert separator
            (define-key haskell-leader-map "ii" 'insert-haskell-comment-separator)

            ;; Go to import list
            (define-key haskell-leader-map "^" 'haskell-navigate-imports)

            ;; Format impor list
            (define-key haskell-leader-map "p" 'haskell-mode-stylish-buffer)

            ;; Type under cursor
            (define-key haskell-leader-map "t" 'haskell-process-do-type)

            ;; Information under cursor
            (define-key haskell-leader-map "d" 'haskell-process-do-info)

            ;; Load file into ghci
            (define-key haskell-leader-map "rr" 'haskell-process-load-file)

            (define-key haskell-leader-map "bd" 'hindent-reformat-buffer)

            ;; Launch repl
            (evil-define-key 'normal haskell-mode-map (kbd "C-c j") 'haskell-process-load-or-reload)

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Configuration

            ;; Text width
            (set-fill-column 80)
            (auto-fill-mode)

            ;; Spell checking
            (flycheck-mode)
            ;; (dante-mode)
            (setq flymake-no-changes-timeout nil)
            (setq flymake-start-syntax-check-on-newline nil)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))))

(provide 'my-haskell)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-haskell.el ends here
