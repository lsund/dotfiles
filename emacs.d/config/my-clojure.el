;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Clojure
;; Some definitions thanks to friemen [https://github.com/friemen]

;;; Code:

(provide 'my-clojure)

(require 'my-lisp)

(require-packages '(paredit
                    paxedit
                    flycheck-clj-kondo
                    clojure-mode
                    clj-refactor
                    evil-cleverparens
                    cider))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cider Mode

;; Keybindings
(eval-after-load "cider-mode"
  '(progn
     (evil-define-key 'normal clojure-mode-map
       (kbd "C-c j") 'cider-jack-in)
     (evil-define-key 'normal clojure-mode-map
       (kbd "C-c l") 'sesman-link-with-buffer)
     (evil-define-key 'normal clojure-mode-map
       (kbd "C-c r") 'cider-restart)
     (evil-define-key 'normal clojure-mode-map
       (kbd "C-c f") 'cider-format-buffer)
     (evil-define-key 'normal clojure-mode-map
       (kbd "C-c n") 'cider-repl-set-ns)
     ;; Cider Repl
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "C-p") 'cider-repl-previous-input)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "C-n") 'cider-repl-next-input)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "C-u") 'cider-repl-kill-input)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "C-a") 'back-to-indentation)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "C-e") 'end-of-line)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "C-h") 'delete-backward-char)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "C-w") 'backward-kill-word)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "M-f") 'forward-word)
     (evil-define-key 'insert cider-repl-mode-map
       (kbd "M-b") 'backward-word)))

;; Use smartparens in the cider repl
(add-hook 'cider-repl-mode-hook 'smartparens-mode)
(add-hook 'cider-repl-mode-hook 'evil-cleverparens-mode)

;; Dont pop up cider repl on connect
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Start in insert mode
(evil-set-initial-state 'cider-repl 'insert)

;; Pretty print
(cider-repl-toggle-pretty-printing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure Mode
(add-hook 'clojure-mode-hook
          (lambda ()
            (interactive)


            (define-key my-leader-map "aa" (lambda ()
                                             (interactive)
                                             (counsel-ag ""
                                                         (locate-dominating-file
                                                          default-directory
                                                          "project.clj"))))


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
            (define-key evil-normal-state-map (kbd "L") 'evil-cp-forward-sexp)
            (define-key evil-normal-state-map (kbd "H") 'evil-cp-backward-sexp)

            (local-unset-key (kbd "M-l"))

            ;; Lambdawerk

            (add-hook 'before-save-hook 'lambdawerk-cleanup-buffer t t)

            ;; CIDER

            (define-key clojure-leader-map "d" 'cider-debug-defun-at-point)
            (define-key clojure-leader-map "ce" 'cider-eval-last-sexp)
            (define-key clojure-leader-map "cr" 'cider-eval-last-sexp-to-repl)
            (define-key evil-normal-state-map (kbd "C-]") 'cider-find-var)

            (define-key clojure-leader-map "R"
              (lambda ()
                (interactive)
                (cider-interactive-eval
                 "(user/system-restart!)")))

            (define-key clojure-leader-map "v"
              (lambda ()
                (interactive)
                (cider-interactive-eval
                 "(do (require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh-all))")))))

(diminish 'projectile-mode)
(diminish 'auto-fill-mode)
(diminish 'autospace-mode)
(diminish 'whitespace-mode)
(diminish 'cider-mode)
(diminish 'projectile-mode)
(diminish 'flycheck-mode)
(diminish 'ivy-mode)
(diminish 'smartparens-mode)
(diminish 'company-mode)
(diminish 'drag-stuff-mode)
(diminish 'undo-tree-mode)
(diminish 'auto-revert-mode)
(diminish 'linum-relative-mode)
(diminish 'evil-cleverparens-mode)
(diminish 'eldoc-mode)
(diminish 'evil-commentary-mode)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-clojure.el ends here
