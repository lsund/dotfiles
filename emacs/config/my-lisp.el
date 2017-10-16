;;; package --- Summary
(provide 'my-lisp)

;;; Commentary:
;;; My Emacs config for All Lisp

;;; Code:

(require-packages '( ))

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

(defun my-clojure-indent-defn ()
  "Todo."
  (interactive)
  (save-excursion
    (goto-char (my-beginning-of-form))
    (indent-sexp)
    (clojure-align (point) (my-end-of-form))))

(defun my-break-sexp ()
  "Todo."
  (interactive)
  (evil-cp-forward-sexp)
  (open-line 1)
  (my-clojure-indent-defn))

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
