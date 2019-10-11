[1mdiff --git a/emacs b/emacs[m
[1mindex 7240229..a26d0e2 100644[m
[1m--- a/emacs[m
[1m+++ b/emacs[m
[36m@@ -22,6 +22,7 @@[m
  '(evil-cleverparens-use-regular-insert t)[m
  '(evil-search-module (quote evil-search))[m
  '(indent-tabs-mode nil)[m
[32m+[m[32m '(lsp-ui-doc-enable nil)[m
  '(package-selected-packages[m
    (quote[m
     (company-lsp lsp-ui lsp-mode use-package meghanada meghanada-mode flycheck-meghanada flycheck-java ggtags solarized-theme solarized-emacs flycheck-clj-kondo rust-mode flycheck-rust hindent yaml-mode web-mode xquery-mode tidy diminish airline-themes cider-repl wrap-region color-theme-solarized blackboard-theme zenburn-theme counsel org-bullets haskell-mode shakespeare-mode paxedit clj-refactor markdown-mode flycheck smex neotree evil-magit swiper flx-ido evil-surround cider evil-cleverparens lispy company evil-mc auto-complete evil-commentary evil-nerd-commenter multiple-cursors highlight-symbol badwolf-theme centered-cursor-mode helm-config drag-stuff evil-quickscope rainbow-delimiters swiper-helm helm-ag helg-ag magit projectile paredit evil-smartparens smartparens tabbar clojure-mode powerline linum-relative helm)))[m
[1mdiff --git a/emacs.d/config/my-clojure.el b/emacs.d/config/my-clojure.el[m
[1mindex 23bdb4a..43366d5 100644[m
[1m--- a/emacs.d/config/my-clojure.el[m
[1m+++ b/emacs.d/config/my-clojure.el[m
[36m@@ -73,24 +73,6 @@[m
           (lambda ()[m
             (interactive)[m
 [m
[31m-            (diminish 'projectile-mode)[m
[31m-            (diminish 'auto-fill-mode)[m
[31m-            (diminish 'autospace-mode)[m
[31m-            (diminish 'whitespace-mode)[m
[31m-            (diminish 'cider-mode)[m
[31m-            (diminish 'projectile-mode)[m
[31m-            (diminish 'flycheck-mode)[m
[31m-            (diminish 'ivy-mode)[m
[31m-            (diminish 'smartparens-mode)[m
[31m-            (diminish 'company-mode)[m
[31m-            (diminish 'drag-stuff-mode)[m
[31m-            (diminish 'undo-tree-mode)[m
[31m-            (diminish 'auto-revert-mode)[m
[31m-            (diminish 'linum-relative-mode)[m
[31m-            (diminish 'evil-cleverparens-mode)[m
[31m-            (diminish 'eldoc-mode)[m
[31m-            (diminish 'evil-commentary-mode)[m
[31m-[m
 [m
             (define-key my-leader-map "aa" (lambda ()[m
                                              (interactive)[m
[36m@@ -158,6 +140,24 @@[m
                 (cider-interactive-eval[m
                  "(do (require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh-all))")))))[m
 [m
[32m+[m[32m(diminish 'projectile-mode)[m
[32m+[m[32m(diminish 'auto-fill-mode)[m
[32m+[m[32m(diminish 'autospace-mode)[m
[32m+[m[32m(diminish 'whitespace-mode)[m
[32m+[m[32m(diminish 'cider-mode)[m
[32m+[m[32m(diminish 'projectile-mode)[m
[32m+[m[32m(diminish 'flycheck-mode)[m
[32m+[m[32m(diminish 'ivy-mode)[m
[32m+[m[32m(diminish 'smartparens-mode)[m
[32m+[m[32m(diminish 'company-mode)[m
[32m+[m[32m(diminish 'drag-stuff-mode)[m
[32m+[m[32m(diminish 'undo-tree-mode)[m
[32m+[m[32m(diminish 'auto-revert-mode)[m
[32m+[m[32m(diminish 'linum-relative-mode)[m
[32m+[m[32m(diminish 'evil-cleverparens-mode)[m
[32m+[m[32m(diminish 'eldoc-mode)[m
[32m+[m[32m(diminish 'evil-commentary-mode)[m
[32m+[m
 ;; Local Variables:[m
 ;; byte-compile-warnings: (not free-vars)[m
 ;; End:[m
[1mdiff --git a/emacs.d/config/my-config.el b/emacs.d/config/my-config.el[m
[1mindex 1c5aeca..78d50bf 100644[m
[1m--- a/emacs.d/config/my-config.el[m
[1m+++ b/emacs.d/config/my-config.el[m
[36m@@ -155,6 +155,7 @@[m
 (diminish 'haskell-mode)[m
 (diminish 'interactive-haskell-mode)[m
 (diminish 'projectile-mode)[m
[32m+[m[32m(diminish 'lsp-mode)[m
 [m
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[m
 ;; Visuals[m
[1mdiff --git a/emacs.d/config/my-scala.el b/emacs.d/config/my-scala.el[m
[1mindex 29f0b6d..2962a96 100644[m
[1m--- a/emacs.d/config/my-scala.el[m
[1m+++ b/emacs.d/config/my-scala.el[m
[36m@@ -12,11 +12,6 @@[m
 [m
 ;; (require-packages '(scala-mode))[m
 [m
[31m-;; Install use-package if not already installed[m
[31m-(unless (package-installed-p 'use-package)[m
[31m-  (package-refresh-contents)[m
[31m-  (package-install 'use-package))[m
[31m-[m
 (require 'use-package)[m
 [m
 ;; Enable defer and ensure by default for use-package[m
[36m@@ -29,26 +24,19 @@[m
 (use-package sbt-mode[m
   :commands sbt-start sbt-command[m
   :config[m
[31m-  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31[m
[31m-  ;; allows using SPACE when in the minibuffer[m
   (substitute-key-definition[m
    'minibuffer-complete-word[m
    'self-insert-command[m
    minibuffer-local-completion-map))[m
 [m
[31m-[m
[31m-;; Enable nice rendering of diagnostics like compile errors.[m
 (use-package flycheck[m
   :init (global-flycheck-mode))[m
 [m
 (use-package lsp-mode[m
[31m-  ;; Optional - enable lsp-mode automatically in scala files[m
   :hook (scala-mode . lsp)[m
   :config (setq lsp-prefer-flymake nil))[m
 [m
 (use-package lsp-ui)[m
[31m-[m
[31m-;; Add company-lsp backend for metals[m
 (use-package company-lsp)[m
 [m
 (add-hook 'scala-mode-hook[m
[36m@@ -61,6 +49,9 @@[m
                 map))[m
             (define-key evil-normal-state-map "\\" scala-leader-map)[m
             (define-key scala-leader-map "bd" 'lsp-format-buffer)[m
[32m+[m[32m            (evil-define-key 'normal scala-mode-map (kbd "C-c C-d C-d") 'lsp-ui-doc-show)[m
             ))[m
 [m
[32m+[m[32m(diminish 'lsp-mode)[m
[32m+[m
 ;;; my-scala.el ends here[m
