;;; package --- Summary

;;; Commentary:
;;; My Emacs config for editing Emacs Lisp

(provide 'my-elisp)
(require 'my-lisp)

(require-packages '(
		    evil
		    evil-cleverparens
		    flycheck
		    ))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax checker

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save new-line))

(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (interactive)

	    (defvar elisp-leader-map
	      (let ((map (make-sparse-keymap)))

		(set-keymap-parent map my-leader-map)
		map))

	    (setq evil-cleverparens-use-additional-bindings nil)
	    (evil-define-key 'normal emacs-lisp-mode-map "\\" elisp-leader-map)

	    (define-key elisp-leader-map "r" 'eval-defun)
	    (define-key elisp-leader-map "ii" 'insert-lisp-comment-separator)
	    (define-key elisp-leader-map (kbd "bd") 'my-clojure-indent-defn)
	    (define-key elisp-leader-map (kbd "bk") 'my-join-sexp)))

;;; my-elisp ends here
