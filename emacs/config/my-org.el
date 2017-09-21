;;; package --- Summary
;;; My org-mode configuration

;;; Commentary:

;;; Code:


(require-packages '(

		    org
		    org-bullets

		    ))



(setq org-support-shift-select t)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DONE")))

(setq org-log-done t)

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq org-time-stamp-formats
		  '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
	    (org-bullets-mode 1)))

(setq org-ellipsis " ↴")

(setq org-agenda-files (list "~/Documents/org/todo.org"))

(define-key evil-normal-state-map (kbd "C-c a") 'org-agenda)
(define-key evil-normal-state-map (kbd "C-c s") 'org-schedule)
(define-key evil-normal-state-map (kbd "C-c q") 'org-set-tags)
(define-key minibuffer-local-map (kbd "C-h") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key minibuffer-local-map (kbd "C-l") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key minibuffer-local-map (kbd "C-j") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
(define-key minibuffer-local-map (kbd "C-k") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))

(define-key org-mode-map (kbd "C-l") 'org-timestamp-up-day)
(define-key org-mode-map (kbd "C-h") 'org-timestamp-down-day)

(add-hook 'org-agenda-mode-hook
          (lambda ()
	    (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
	    (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)
	    (define-key org-agenda-mode-map (kbd "l") 'right-char)
	    (define-key org-agenda-mode-map (kbd "h") 'left-char)
	    (define-key org-agenda-mode-map (kbd "e") 'right-word)
	    (define-key org-agenda-mode-map (kbd "b") 'left-word)
	    (define-key org-agenda-mode-map (kbd "M-h") 'windmove-left)
	    (define-key org-agenda-mode-map (kbd "M-l") 'windmove-right)
	    (define-key org-agenda-mode-map (kbd "M-j") 'windmove-down)
	    (define-key org-agenda-mode-map (kbd "M-k") 'windmove-up)))

(provide 'my-org)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-org.el ends here
