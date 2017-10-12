;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Clojure
;; Some definitions thanks to friemen [https://github.com/friemen]

;;; Code:

(require-packages '(
		    clojure-mode
		    cider
		    smartparens
		    paredit
		    paxedit
		    rainbow-delimiters
		    evil-cleverparens
		    ))

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
;; Cider Mode

(setq cider-repl-pop-to-buffer-on-connect nil)
(cider-repl-toggle-pretty-printing)
(evil-set-initial-state 'cider-repl 'insert)

(with-eval-after-load "cider-mode"
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "C-a") 'back-to-indentation)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "C-e") 'evil-end-of-line)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "C-p") 'cider-repl-previous-input)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "C-u") 'cider-repl-kill-input)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "C-h") 'delete-backward-char)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "C-w") 'backward-kill-word)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "M-f") 'forward-word)
  (evil-define-key 'insert
    cider-repl-mode-map (kbd "M-b") 'backward-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure Mode
(add-hook 'clojure-mode-hook

	  (lambda ()
	    (interactive)

	    ;; Leader map extension
	    (defvar clojure-leader-map
	      (let ((map (make-sparse-keymap)))
		(set-keymap-parent map my-leader-map)
		map))

	    (evil-define-key 'normal clojure-mode-map "\\" clojure-leader-map)

	    (define-key clojure-leader-map "r" #'cider-eval-buffer)
	    (define-key clojure-leader-map "ii" 'insert-lisp-comment-separator)
	    (define-key clojure-leader-map (kbd "bb") 'my-break-sexp)
	    (define-key clojure-leader-map (kbd "bk") 'my-join-sexp)
	    (define-key clojure-leader-map (kbd "bd") 'my-clojure-indent-defn)

	    ;; Other extensions
	    (evil-define-key 'normal clojure-mode-map
	      (kbd "C-c r") 'cider-restart)
	    (evil-define-key 'normal clojure-mode-map
	      (kbd "C-c C-l") 'cider-jack-in)
	    (evil-define-key 'normal clojure-mode-map
	      (kbd "C-c f") 'cider-format-buffer)

	    (setq evil-cp-additional-bindings
  		  '(("M-t" . sp-transpose-sexp)
  		    ("M-k" . nil)
  		    ("M-j" . nil)
  		    ("M-J" . sp-join-sexp)
  		    ("M-s" . sp-splice-sexp)
  		    ("M-S" . sp-split-sexp)
  		    ("M-R" . evil-cp-raise-form)
  		    ("M-r" . sp-raise-sexp)
  		    ("M-a" . evil-cp-insert-at-end-of-form)
  		    ("M-i" . evil-cp-insert-at-beginning-of-form)
  		    ("M-w" . evil-cp-copy-paste-form)
  		    ("M-y" . evil-cp-yank-sexp)
  		    ("M-d" . evil-cp-delete-sexp)
  		    ("M-c" . evil-cp-change-sexp)
  		    ("M-Y" . evil-cp-yank-enclosing)
  		    ("M-D" . evil-cp-delete-enclosing)
  		    ("M-C" . evil-cp-change-enclosing)
  		    ("M-q" . sp-indent-defun)
  		    ("M-o" . evil-cp-open-below-form)
  		    ("M-O" . evil-cp-open-above-form)
  		    ("M-v" . sp-convolute-sexp)
  		    ("M-(" . evil-cp-wrap-next-round)
  		    ("M-)" . evil-cp-wrap-previous-round)
  		    ("M-[" . evil-cp-wrap-next-square)
  		    ("M-]" . evil-cp-wrap-previous-square)
  		    ("M-{" . evil-cp-wrap-next-curly)
  		    ("M-}" . evil-cp-wrap-previous-curly)))

	    ;; Auto fill
	    (set-fill-column 80)
	    (auto-fill-mode)

	    ;; Rainbow delimiters
	    (rainbow-delimiters-mode)
	    
	    ;; Smartparens
	    (require 'smartparens-config)
	    (sp-pair "'" nil :actions :rem)
	    (smartparens-mode)

	    ;; Evil Cleverparens
	    (evil-cleverparens-mode))

	  (provide 'my-clojure))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-clojure.el ends here
