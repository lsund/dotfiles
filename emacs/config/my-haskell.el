;;; Package --- Summary: My emacs haskell config

(require-packages '(
                    rainbow-delimiters
                    ;; shakespeare-mode
                    haskell-mode
                    ;; intero
                    yaml-mode
                    dante
                    flycheck
                    ))

(require 'my-ghcid)

(require 'haskell-interactive-mode)
(require 'haskell-process)

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

(eval-after-load "haskell-interactive-mode"
  '(progn
     ;; Step back in history
     (evil-define-key 'insert haskell-interactive-mode-map
       (kbd "C-p") 'haskell-interactive-mode-history-previous)
     ;; Step forward in history
     (evil-define-key 'insert haskell-interactive-mode-map
       (kbd "C-n") 'haskell-interactive-mode-history-next)
     ))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            (interactive)

            (set-fill-column 80)
            (auto-fill-mode)

            ;; Spell checking
            (dante-mode)
            (flycheck-mode)

            (setq flymake-no-changes-timeout nil)
            (setq flymake-start-syntax-check-on-newline nil)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))

            ;; Leader map
            (defvar haskell-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" haskell-leader-map)


            ;; Keybindings
            (define-key haskell-leader-map "ii"
              'insert-haskell-comment-separator)

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

            ;; Launch repl
            (evil-define-key 'normal haskell-mode-map
              (kbd "C-c j") 'haskell-process-load-or-reload)


            ))

(provide 'my-haskell)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-haskell.el ends here
