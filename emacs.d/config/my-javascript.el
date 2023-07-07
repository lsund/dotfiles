;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing JavaScript

;;; Code:

(provide 'my-javascript)

(require-packages '(js2-mode
                    js2-refactor
                    xref-js2
                    rjsx-mode
                    ))

(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'rjsx-mode-hook #'origami-mode)

(add-hook 'rjsx-mode-hook
          (lambda ()
            (interactive)
            (defvar javascript-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" javascript-leader-map)
            (define-key javascript-leader-map "bd" 'iwb)))


;; (js2r-add-keybindings-with-prefix "C-c C-r")

;; (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; (use-package dap-java :after (lsp-java))
