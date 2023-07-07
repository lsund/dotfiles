;;; package --- Summary

;; Scala specific config

;;; Commentary:

;;; Code:

(provide 'my-scala)

(require 'package)

;; (require-packages '(scala-mode))

(require 'use-package)

;; Enable defer and ensure by default for use-package
(setq use-package-always-defer t
      use-package-always-ensure t)

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)
(use-package company-lsp)

(add-hook 'scala-mode-hook
          (lambda ()
            (interactive)
            (diminish 'lsp-mode)
            (diminish 'yas-global-mode)
            ;; Keybindings
            (defvar scala-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key evil-normal-state-map "\\" scala-leader-map)
            (define-key scala-leader-map "bd" 'lsp-format-buffer)
            (evil-define-key 'normal scala-mode-map (kbd "C-c C-d C-d") 'lsp-ui-doc-show)
            ))

;;; my-scala.el ends here
