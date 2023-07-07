;;; package --- Summary

;;; Commentary:
;;; My Emacs config for editing Emacs Lisp

;;; Code:

(require 'my-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax checker

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save new-line))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (interactive)

	    (rainbow-delimiters-mode)
            (evil-cleverparens-mode)

            (define-key evil-cleverparens-mode-map (kbd "M-H") 'evil-cp-drag-backward)
            (define-key evil-cleverparens-mode-map (kbd "M-L") 'evil-cp-drag-forward)

            ;; Smartparens
	    (require 'smartparens-config)
	    (sp-pair "'" nil :actions :rem)
	    (smartparens-mode)

            (define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
            (define-key smartparens-mode-map (kbd "M-r") 'sp-raise-sexp)

            (defvar elisp-leader-map
	      (let ((map (make-sparse-keymap)))

		(set-keymap-parent map my-leader-map)
		map))

	    (setq evil-cleverparens-use-additional-bindings nil)
	    (evil-define-key 'normal emacs-lisp-mode-map "\\" elisp-leader-map)

	    (define-key elisp-leader-map "rr" 'eval-defun)
	    (define-key elisp-leader-map "ii" 'insert-lisp-comment-separator)
	    (define-key elisp-leader-map (kbd "bk") 'my-join-sexp)
            (define-key elisp-leader-map (kbd "bb") 'my-break-sexp)))

(provide 'my-elisp)
;;; my-elisp ends here
