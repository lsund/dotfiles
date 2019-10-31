;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Java

;;; Code:

(provide 'my-java)

(require-packages '(lsp-java
                    use-package
                    company-lsp
                    editorconfig
                    lsp-ui
                    lsp-treemacs
                    lsp-mode
                    cc-mode))

(add-hook 'java-mode-hook #'lsp)

(add-hook 'java-mode-hook
          (lambda ()
            (interactive)
            (setq lsp-java-format-settings-url "/home/lsund/.eclipse-formatspec.xml")
            (diminish 'yas-global-mode)
            (push 'company-lsp company-backends)
            (editorconfig-mode 1)
            (setq-default tab-width 4)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Keys
            (defvar java-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" java-leader-map)
            (define-key java-leader-map "c" 'lsp-rename)
            (define-key java-leader-map "bd" 'lsp-format-buffer)
            (evil-define-key
              'normal
              java-mode-map
              (kbd "C-c C-d C-d")
              'lsp-ui-doc-show)))

(add-hook 'java-mode-hook 'flycheck-mode)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(add-hook 'lsp-mode-hook 'lsp-treemacs-mode)
