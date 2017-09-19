;;; package --- Summary
(provide 'my-elisp)

(require-packages '(

		    evil
		    evil-cleverparens

		    flycheck

		    ))

;;; Commentary:
;;; My Emacs config for editing Emacs Lisp

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax checker

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save new-line))

(defun insert-comment-separator ()
  "Insert a comment separator."
  (interactive)
  (insert
   (format "%s%s"
	   ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
	   ";;;;;;;;;;;;"))
  (open-line 1)
  (evil-next-line)
  (insert ";; ")
  (evil-insert 1))


(add-hook 'emacs-lisp-mode-hook 'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (interactive)

	    (defvar elisp-leader-map
	      (let ((map (make-sparse-keymap)))

		(set-keymap-parent map my-leader-map)
		map))

	    (setq evil-cleverparens-use-additional-bindings nil)
	    (define-key evil-normal-state-map "\\" elisp-leader-map)

	    (define-key elisp-leader-map "r" 'eval-defun)
	    (define-key elisp-leader-map "ii" 'insert-comment-separator)

	    ))

;;; my-elisp ends here
