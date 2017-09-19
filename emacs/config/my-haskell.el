(require-packages '(

		    haskell-mode
		    intero

		    ))

;;; Commentary:

;;; Code:


(add-hook 'haskell-mode-hook
	  (lambda ()
	    (interactive)

	    (defvar haskell-leader-map
	      (let ((map (make-sparse-keymap)))
		(set-keymap-parent map my-leader-map)
		map))
	    (define-key evil-normal-state-map "\\" haskell-leader-map)

	    ;; Keybindings
	    (define-key evil-insert-state-map (kbd "C-p")
	      'haskell-interactive-mode-history-previous)
	    (define-key evil-insert-state-map (kbd "C-n")
	      'haskell-interactive-mode-history-next)
	    (define-key evil-insert-state-map (kbd "C-u")
	      'haskell-interactive-mode-kill-whole-line)

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
