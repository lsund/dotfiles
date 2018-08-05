;;; package --- Summary

;;; Commentary:
;;; My Emacs config for editing Emacs Lisp

;;; Code:

(require 'my-lisp)

(require-packages '(
		    evil
		    evil-cleverparens
		    flycheck
		    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax checker

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save new-line))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (interactive)

	    (rainbow-delimiters-mode)
            (evil-cleverparens-mode)

            ;; Smartparens
	    (require 'smartparens-config)
	    (sp-pair "'" nil :actions :rem)
	    (smartparens-mode)

            (defvar elisp-leader-map
	      (let ((map (make-sparse-keymap)))

		(set-keymap-parent map my-leader-map)
		map))

	    (setq evil-cleverparens-use-additional-bindings nil)
	    (evil-define-key 'normal emacs-lisp-mode-map "\\" elisp-leader-map)

	    (define-key elisp-leader-map "rr" 'eval-defun)
	    (define-key elisp-leader-map "ii" 'insert-lisp-comment-separator)
	    (define-key elisp-leader-map (kbd "bd") 'my-clojure-indent-defn)
	    (define-key elisp-leader-map (kbd "bk") 'my-join-sexp)))

(provide 'my-elisp)
;;; my-elisp ends here
