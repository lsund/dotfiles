;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Java

;;; Code:

(provide 'my-java)

(require-packages '(lsp-java
                    use-package
                    company-lsp
                    lsp-ui
                    lsp-mode
                    cc-mode))

(add-hook 'java-mode-hook #'lsp)

(add-hook 'java-mode-hook
          (lambda ()
            (interactive)
            (diminish 'yas-global-mode)
            (push 'company-lsp company-backends)
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

;; (use-package yasnippet :ensure t)
;; (use-package company-lsp :ensure t)

;; (use-package dap-mode
;;   :ensure t :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))
;; (use-package dap-java :after (lsp-java))

;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             ;; meghanada-mode on
;;             (meghanada-mode t)
;;             ;; enable telemetry
;;             (meghanada-telemetry-enable t)
;;             (flycheck-mode +1)
;;             (setq c-basic-offset 2)
;;             ;; use code format
;;             (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;;             (ggtags-mode 1)))

;; (setq meghanada-java-path "/usr/bin/java")
;; (setq meghanada-maven-path "/usr/bin/maven")
