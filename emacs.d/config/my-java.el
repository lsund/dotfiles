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
                    cc-mode
                    flycheck
                    ))

(add-hook 'java-mode-hook #'lsp)

(add-hook 'java-mode-hook
          (lambda ()
            (interactive)
            (setq lsp-java-autobuild-enabled nil)
            (setq lsp-java-format-settings-url "/home/lsund/.eclipse-formatspec.xml")
            (diminish 'yas-global-mode)
            (push 'company-lsp company-backends)
            (editorconfig-mode 1)
            (setq-default tab-width 4)

            ;; TODO

            (flycheck-define-checker checkstyle
              "Checkstyle checker"
              :command ("checkstyle" source-original)
              :standard-input t
              ;; flycheck-parse-checkstyle not to be confused with checkstyle itself
              :error-parser flycheck-parse-checkstyle
              :modes (java-mode))

            (add-to-list 'flycheck-checkers 'checkstyle 'append)

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Keys
            (defvar java-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" java-leader-map)
            (evil-define-key 'normal java-mode-map
              (kbd "C-c r") 'lsp-rename)
            (evil-define-key 'normal java-mode-map
              (kbd "C-c i") 'lsp-java-add-import)
            (define-key java-leader-map "rr" 'lsp-java-build-project)
            (define-key java-leader-map "bd" 'lsp-format-buffer)
            (evil-define-key
              'normal
              java-mode-map
              (kbd "C-c C-d C-d")
              'lsp-ui-doc-show)))

(add-hook 'java-mode-hook 'flycheck-mode)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
