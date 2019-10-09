;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Java

;;; Code:

(provide 'my-java)

(require-packages '(meghanada))

(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            ;; enable telemetry
            (meghanada-telemetry-enable t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
            (ggtags-mode 1)))

(setq meghanada-java-path "/usr/bin/java")
(setq meghanada-maven-path "/usr/bin/maven")
