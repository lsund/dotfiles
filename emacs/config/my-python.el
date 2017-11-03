;;; package --- Summary

;;; Commentary:
;;; My Emacs config for editing Python

(require-packages '(
		    flycheck
		    ))

(add-hook 'python-mode-hook
          (lambda ()
            (interactive)

            ;; (add-hook 'after-save-hook
            ;;           (lambda ()
            ;;             (mark-whole-buffer)
            ;;             (untabify)))

            (add-hook 'after-init-hook
                      #'global-flycheck-mode)
            (setq flycheck-check-syntax-automatically '(save new-line))

            ;; Auto fill
            (set-fill-column 80)
            (auto-fill-mode)

            ;; Manual tabs
            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq indent-line-function 'tab-to-tab-stop)
            (setq tab-stop-list (number-sequence 4 120 4))


            )
          )

(provide 'my-python)
;;; my-python.el ends here
