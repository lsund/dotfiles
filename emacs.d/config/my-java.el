;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Java

;;; Code:

(provide 'my-java)

(require-packages '(lsp-java
                    use-package
                    dap-java
                    cc-mode))

(use-package yasnippet :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui :ensure t)
(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))

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
