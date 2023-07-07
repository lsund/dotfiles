;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing Java

;;; Code:

(provide 'my-java)

(require-packages '(lsp-java
                    use-package
                    company-lsp
                    editorconfig
                    dap-mode
                    lsp-ui
                    lsp-treemacs
                    lsp-mode
                    cc-mode
                    flycheck
                    yasnippet
                    ))

(require 'dap-java)

(add-hook 'java-mode-hook #'lsp)

(add-hook 'java-mode-hook
          (lambda ()
            (interactive)
            (setq lsp-java-autobuild-enabled nil)
            (setq lsp-java-format-settings-url "/home/lsund/.eclipse-formatspec.xml")
            (setq path-to-lombok "/home/lsund/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar")
            (setq lsp-java-vmargs
                  `("-noverify"
                    "-Xmx1G"
                    "-XX:+UseG1GC"
                    "-XX:+UseStringDeduplication"
                    ,(concat "-javaagent:" path-to-lombok)
                    ;; ,(concat "-Xbootclasspath/a:" path-to-lombok)
                    )
                  )

            (diminish 'yas-minor-mode)
            (diminish 'lsp-mode)
            (diminish 'lsp-ui-mode)
            (diminish 'editorconfig-mode)
            (diminish 'editorconfig-conf-mode)
            (diminish 'abbrev-mode)
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
            (evil-define-key 'normal java-mode-map
              (kbd "C-c c") 'lsp-execute-code-action)
            (define-key java-leader-map "rr" 'lsp-java-build-project)
            (define-key java-leader-map "bd" 'lsp-format-buffer)
            (define-key java-leader-map "nm" 'lsp-ui-imenu)
            (evil-define-key
              'normal
              java-mode-map
              (kbd "C-c C-d C-d")
              'lsp-ui-doc-show)))

(add-hook 'java-mode-hook 'flycheck-mode)

;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'dap-mode)

(add-hook 'dap-mode-hook 'dap-ui-mode)


;; (use-package dap-java :after (lsp-java))
