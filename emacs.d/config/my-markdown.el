;;; package --- Summary: My emacs markdown config.

;;; Commentary:

;;; Code:

(require-packages '(markdown-mode))

(add-hook 'markdown-mode-hook

	  (lambda ()
	    (interactive)

	    ;; Other extensions
	    (evil-define-key 'normal markdown-mode-map
	      (kbd "<S-tab>") 'markdown-hide-subtree)
	    (evil-define-key 'normal markdown-mode-map
	      (kbd "TAB") 'markdown-show-subtree)

	    (define-key evil-normal-state-map
	      (kbd "TAB") 'markdown-show-subtree)

	    ;; Auto fill
	    (set-fill-column 80)
	    (auto-fill-mode)))

(provide 'my-markdown)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-markdown.el ends here
