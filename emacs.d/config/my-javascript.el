;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing JavaScript

;;; Code:

(provide 'my-java)

(require-packages '(js2-mode
                    js2-refactor
                    xref-js2
                    rjsx-mode
                    ))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; (js2r-add-keybindings-with-prefix "C-c C-r")

;; (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; (use-package dap-java :after (lsp-java))
