;;; package --- Summary

;; My Emacs config for editing Clojure
;; Some definitions thanks to friemen [https://github.com/friemen]

(require-packages '(

		    clojure-mode
		    cider
		    smartparens
		    paredit
		    paxedit
		    rainbow-delimiters
		    evil-cleverparens
		    ;; lispy
		    ;; lispyville

		    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent

(defun my-goto-end-of-form-rec (p)
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
  (interactive)
  (my-goto-end-of-form-rec (point)))


(defun my-end-of-form ()
  (save-excursion
    (my-goto-end-of-form)
    (point)))

(defun my-beginning-of-form ()
  (save-excursion
    (my-goto-end-of-form)
    (paredit-backward)
    (point)))

(defun my-clojure-indent-defn ()
  (interactive)
  (save-excursion
    (goto-char (my-beginning-of-form))
    (indent-sexp)
    (clojure-align (point) (my-end-of-form))))

(defun my-break-sexp ()
  (interactive)
  (evil-cp-forward-sexp)
  (open-line 1)
  (my-clojure-indent-defn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smartparens

(require 'smartparens-config)
(sp-pair "'" nil :actions :rem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cider

(setq cider-repl-pop-to-buffer-on-connect nil)
(cider-repl-toggle-pretty-printing)
(evil-set-initial-state 'cider-repl 'insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)

(defun insert-comment-separator ()
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

;; keybindings
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (interactive)
	    (defvar clojure-leader-map
	      (let ((map (make-sparse-keymap)))
		(set-keymap-parent map my-leader-map)
		map))
	    (define-key evil-normal-state-map "\\" clojure-leader-map)

	    (define-key clojure-leader-map "r" #'cider-eval-buffer)
	    (define-key clojure-leader-map "ii" 'insert-comment-separator)
	    (define-key clojure-leader-map (kbd "bb") 'my-break-sexp)
	    (define-key clojure-leader-map (kbd "bi") 'my-clojure-indent-defn)

	    (define-key evil-normal-state-map
	      (kbd "C-c r") 'cider-restart)
	    (define-key evil-normal-state-map
	      (kbd "C-c j") 'cider-jack-in)
	    (define-key evil-normal-state-map
	      (kbd "C-c f") 'cider-format-buffer)


	    ;; (lispy-mode 1)

	    (evil-cleverparens-mode)

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
  		    ("M-}" . evil-cp-wrap-previous-curly))
  		  "Alist containing additional functionality for
  evil-cleverparens via a modifier key (using the meta-key by
  default). Only enabled in evil's normal mode.")

	    )

	  (provide 'my-clojure))
;;; my-clojure.el ends here
