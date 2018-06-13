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

            (setq evil-cleverparens-use-regular-insert)

            ))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-clojure.el ends here
