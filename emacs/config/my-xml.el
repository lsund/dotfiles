;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing XML

;;; Code:

(add-hook 'nxml-mode-hook

          (lambda ()
            (interactive)
            (define-key evil-insert-state-map (kbd "TAB") 'completion-at-point)

            (set-fill-column 80)

            ))



;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-xml.el ends here
