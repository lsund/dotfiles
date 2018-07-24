;; package --- Summary
;;; Top level config file

;;; Commentary:

;;; Code:

;; Counsel
(define-key my-leader-map "o" 'ido-find-file)
(define-key ido-file-completion-map (kbd "C-w") 'backward-kill-word)
(define-key ido-file-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-file-completion-map (kbd "C-p") 'ido-prev-match)
(define-key my-leader-map "a" (lambda () (interactive)
				(counsel-ag "" (vc-root-dir))))

;; Ivy
(defun my-ivy ()
  (interactive)
  (if (ivy-partial)
      nil
    (ivy-next-line)))

(defun my-ivy-back ()
  (interactive)
  (if (ivy-partial)
      nil
    (ivy-previous-line)))

(define-key ivy-minibuffer-map (kbd "C-w") #'backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-h") #'backward-delete-char)
(define-key ivy-minibuffer-map (kbd "<tab>") 'my-ivy)
(define-key ivy-minibuffer-map (kbd "<backtab>") 'my-ivy-back)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'my-ivy)
;;; my-ivy.el ends here
