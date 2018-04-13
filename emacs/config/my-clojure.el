;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Clojure
;; Some definitions thanks to friemen [https://github.com/friemen]

;;; Code:

(provide 'my-clojure)

(require 'my-lisp)

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
  (evil-define-key
    'insert
    cider-repl-mode-map
    (kbd "M-b") 'backward-word)

  (evil-define-key 'normal clojure-mode-map
    (kbd "C-c r") 'cider-restart)
  (evil-define-key 'normal clojure-mode-map
    (kbd "C-c C-l") 'cider-jack-in)
  (evil-define-key 'normal clojure-mode-map
    (kbd "C-c f") 'cider-format-buffer))

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
	    (define-key clojure-leader-map (kbd "a")

	      (lambda ()
		(interactive)
		(counsel-ag "" (concat (vc-root-dir) "src"))))

            (define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)

	    ;; Other extensions

	  ;;    (setq evil-cp-additional-bindings
  	  ;;          '(("M-t" . sp-transpose-sexp)
  	  ;;            ("M-k" . nil)
  	  ;;            ("M-j" . nil)
  	  ;;            ("M-J" . sp-join-sexp)
  	  ;;            ("M-s" . sp-splice-sexp)
  	  ;;            ("M-S" . sp-split-sexp)
  	  ;;            ("M-R" . evil-cp-raise-form)
  	  ;;            ("M-r" . sp-raise-sexp)
  	  ;;            ("M-a" . evil-cp-insert-at-end-of-form)
  	  ;;            ("M-i" . evil-cp-insert-at-beginning-of-form)
  	  ;;            ("M-w" . evil-cp-copy-paste-form)
  	  ;;            ("M-y" . evil-cp-yank-sexp)
  	  ;;            ("M-d" . evil-cp-delete-sexp)
  	  ;;            ("M-c" . evil-cp-change-sexp)
  	  ;;            ("M-Y" . evil-cp-yank-enclosing)
  	  ;;            ("M-D" . evil-cp-delete-enclosing)
  	  ;;            ("M-C" . evil-cp-change-enclosing)
  	  ;;            ("M-q" . sp-indent-defun)
  	  ;;            ("M-o" . evil-cp-open-below-form)
  	  ;;            ("M-O" . evil-cp-open-above-form)
  	  ;;            ("M-v" . sp-convolute-sexp)
  	  ;;            ("M-(" . evil-cp-wrap-next-round)
  	  ;;            ("M-)" . evil-cp-wrap-previous-round)
  	  ;;            ("M-[" . evil-cp-wrap-next-square)
  	  ;;            ("M-]" . evil-cp-wrap-previous-square)
  	  ;;            ("M-{" . evil-cp-wrap-next-curly)
          ;;    ("M-}" . evil-cp-wrap-previous-curly)
          ;;    )
          ;; )

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
	    (evil-cleverparens-mode)

            ))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-clojure.el ends here
