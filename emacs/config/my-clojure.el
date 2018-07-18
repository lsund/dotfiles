;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Clojure
;; Some definitions thanks to friemen [https://github.com/friemen]

;;; Code:

(provide 'my-clojure)

(require 'my-lisp)

(require-packages '(
		    clojure-mode
                    clj-refactor
                    cider
		    paredit
		    paxedit
		    rainbow-delimiters
		    smartparens
		    evil-cleverparens

                    lispy
		    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions


(defun cider-print-docstring ()
  (interactive
   (cider-repl--emit-interactive-output
    (concat
     (cider-interactive-eval
      (format
       "(clojure.repl/doc %s)"
       (symbol-at-point)) nil) "\n") :nil)
   (other-window -1)))

(defun lambdawerk-cleanup-buffer ()
	"clean up buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun lambdawerk-indent ()
	(define-clojure-indent
		(defui '(1 nil nil (1)))
		(with 'defun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cider Mode

(setq cider-repl-pop-to-buffer-on-connect nil)
(cider-repl-toggle-pretty-printing)
(evil-set-initial-state 'cider-repl 'insert)

(eval-after-load "cider-mode"
  '(progn
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
      (kbd "C-c f") 'cider-format-buffer)

    (local-set-key (kbd "C-c d") #'cider-print-docstring)

    )

  )

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

	    (define-key clojure-leader-map "rr" #'cider-eval-buffer)
	    (define-key clojure-leader-map "pp" #'cider-format-buffer)
	    (define-key clojure-leader-map "ii" 'insert-lisp-comment-separator)
	    (define-key clojure-leader-map (kbd "bb") 'my-break-sexp)
	    (define-key clojure-leader-map (kbd "bk") 'my-join-sexp)
	    (define-key clojure-leader-map (kbd "bd") 'my-clojure-indent-defn)
	    (define-key clojure-leader-map (kbd "a")
	      (lambda ()
		(interactive)
		(counsel-ag "" (concat (vc-root-dir) "src"))))

            ;; (clj-refactor-mode 1)
            ;; (yas-minor-mode 1) ; for adding require/use/import statements
            ;; ;; This choice of keybinding leaves cider-macroexpand-1 unbound
            ;; (cljr-add-keybindings-with-prefix "C-c C-m")

	    ;; Other extensions

            ;; Auto fill
	    (set-fill-column 80)
	    (auto-fill-mode)

	    ;; Rainbow delimiters
	    (rainbow-delimiters-mode)

            (define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
            (define-key smartparens-mode-map (kbd "M-r") 'sp-raise-sexp)

	    ;; Smartparens
	    (require 'smartparens-config)
	    (sp-pair "'" nil :actions :rem)
	    (smartparens-mode)

	    ;; Evil Cleverparens
	    (evil-cleverparens-mode)

            (define-key evil-cleverparens-mode-map (kbd "M-H") 'evil-cp-drag-backward)
            (define-key evil-cleverparens-mode-map (kbd "M-L") 'evil-cp-drag-forward)

            (local-unset-key (kbd "M-l"))

            (setq evil-cleverparens-use-regular-insert)

            ;; (setq  evil-cleverparens-use-additional-movement-keys false)

	    (setq evil-cp-additional-movement-keys
  	          '(("M-l" . nil)
  	            ("M-h" . nil)
                    )
                  )

            (setq evil-cp-additional-bindings
  	          '(("M-t" . sp-transpose-sexp)
  	            ("M-k" . evil-cp-drag-forward)
  	            ("M-j" . evil-cp-drag-backward)
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
                    ("M-}" . evil-cp-wrap-previous-curly)
                    )
                  )

            ;; Lambdawerk

            (lambdawerk-indent)
            (add-hook 'before-save-hook 'lambdawerk-cleanup-buffer t t)


            ))

(eval-after-load "clojure-mode"
  '(define-key evil-cleverparens-mode-map (kbd "M-l") nil))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-clojure.el ends here
