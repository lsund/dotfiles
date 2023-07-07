;;; package --- Summary
;;; My org-mode configuration

;;; Commentary:

;;; Code:


(require-packages '(
		    org
		    org-bullets
		    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory/filenames

(setq org-directory "~/Documents/org")
(setq org-agenda-files (list "~/Documents/org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Archiving

(defun mark-done-and-archive ()
  "Mark the state of an 'org-mode' item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name.
FILENAME: The filename."
  (concat (file-name-as-directory org-directory) filename))

(setq org-archive-location
      (concat (org-file-path "archive/%s_archive") "::* From %s"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(setq org-ellipsis " ↴")

(setq org-support-shift-select t)

(setq org-todo-keywords
      '((sequence "TODO" "DONE")))

;; Log the time something was done
(setq org-log-done t)

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq org-time-stamp-formats
		  '("<%Y-%m-%d %a>" . "<%Y-%m-%d %a %H:%M>"))
	    (org-bullets-mode 1)))

(eval-after-load "org"
  '(require 'ox-md nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings

(add-hook 'org-agenda-mode-hook
          (lambda ()
	    ;; Navigate agenda
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

(add-hook 'org-mode-hook
	  (lambda ()


	    ;; General
	    (evil-define-key 'normal org-mode-map (kbd "C-c r") 'mark-done-and-archive)
	    (evil-define-key 'normal org-mode-map (kbd "C-c a") 'org-agenda)
	    (evil-define-key 'normal org-mode-map (kbd "C-c s") 'org-schedule)
	    (evil-define-key 'normal org-mode-map (kbd "C-c q") 'org-set-tags)

	    ;; Navigate the calendar
	    (define-key
	      minibuffer-local-map
	      (kbd "C-h")
	      (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
	    (define-key
	      minibuffer-local-map
	      (kbd "C-l")
	      (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
	    (define-key minibuffer-local-map
	      (kbd "C-j")
	      (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
	    (define-key
	      minibuffer-local-map
	      (kbd "C-k")
	      (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))

	    ;; Navigate timestamps
	    (define-key org-mode-map (kbd "C-l") 'org-timestamp-up-day)
	    (define-key org-mode-map (kbd "C-h") 'org-timestamp-down-day)))

(provide 'my-org)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-org.el ends here
