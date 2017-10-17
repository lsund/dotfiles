;;; package --- Summary

;;; Commentary:
;;; My Emacs config for editing Python

(provide 'my-python)

(require-packages '(
		    flycheck
		    ))

(add-hook 'python-mode-hook
          (lambda ()
            (interactive)

            (add-hook 'after-save-hook
                      (mark-whole-buffer)
                      (untabify))

            ;; Auto fill
            (set-fill-column 80)
            (auto-fill-mode)

            ;; Manual tabs
            (setq-local indent-tabs-mode t)
            (setq-default indent-tabs-mode t)
            (setq-default tab-width 4)
            (setq tab-width 4)
            (setq c-basic-indent 4)
            (define-key python-mode-map (kbd "TAB") 'self-insert-command)
            (setq c-backspace-function 'backward-delete-char)
            )
          )
