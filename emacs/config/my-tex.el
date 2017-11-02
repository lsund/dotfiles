;;; Package --- Summary: My emacs tex config

;;; Commentary:

;;; Code:

(defun insert-tex-comment-separator ()
  "Insert a comment separator."
  (interactive)
  (insert
   (format "%s%s"
	   "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	   "%%%%%%%%%%%%"))
  (open-line 1)
  (evil-next-line)
  (insert "%% ")
  (evil-insert 1))

(add-hook 'tex-mode-hook
          (lambda ()
            (interactive)

            ;; Leader map
            (defvar tex-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" tex-leader-map)

            ;; Auto fill
            (set-fill-column 80)
            (auto-fill-mode)

            ;; Keybindings
            (define-key tex-leader-map "ii"
              'insert-tex-comment-separator)

            (define-key tex-leader-map (kbd "r")
              (lambda ()
                (interactive)
                (shell-command "pdflatex report.tex")))
            ))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'my-tex)
;;; my-tex.el ends here
