;;; package --- Summary

;;; Commentary:
;;; My Emacs config for All Lisp

;;; Code:

(require-packages '(rainbow-delimiters
                    smartparens
                    evil-cleverparens))

(defun insert-lisp-comment-separator ()
  "Insert a comment separator."
  (interactive)
  (insert
   (format "%s%s"
	   ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
	   ";;;;;;;;;;;;"))
  (open-line 1)
  (evil-next-line)
  (insert ";; ")
  (evil-insert 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent

(defun my-goto-end-of-form-rec (p)
  "Todo (as P)."
  (let ((next-pos (paxedit-sexp-move-to-core-start)))
    (cond ((looking-at ".comment")
           (progn (message "commment")
                  (goto-char p)
                  (paredit-forward)))

          ((numberp next-pos)
           (my-goto-end-of-form-rec next-pos))

          (t
           (paredit-forward)))))

(defun my-goto-end-of-form ()
  "Todo."
  (interactive)
  (my-goto-end-of-form-rec (point)))


(defun my-end-of-form ()
  "Todo."
  (save-excursion
    (my-goto-end-of-form)
    (point)))

(defun my-beginning-of-form ()
  "Todo."
  (save-excursion
    (my-goto-end-of-form)
    (paredit-backward)
    (point)))

(defun my-break-sexp ()
  "Break the line after the current sexp and move to the next sexp."
  (interactive)
  (evil-cp-forward-sexp)
  (open-line 1)
  (evil-cp-forward-sexp)
  (evil-backward-char)
  (iwb))

(defun my-join-sexp ()
  "Todo."
  (interactive)
  (evil-first-non-blank)
  (evil-insert 1)
  (evil-cp-delete-backward-word)
  (evil-delete-backward-char-and-join 1)
  (insert " ")
  (evil-normal-state)
  (delete-horizontal-space))

(provide 'my-lisp)
;;; my-lisp ends here
