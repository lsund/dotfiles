(provide 'my-clojure)

;; My Emacs config for editing Clojure
;; Some definitions thanks to friemen [https://github.com/friemen]

(require-packages '(

		    clojure-mode
		    cider
		    company
		    smartparens
		    paredit
		    paxedit
		    rainbow-delimiters

		    ))

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

;; Smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(sp-pair "'" nil :actions :rem)

;; Rainbow delimiters
(rainbow-delimiters-mode 1)

;; Hooks
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
