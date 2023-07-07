;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq backup-directory-alist `(("." . "/home/lsund/.emacs-backup-files")))
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-lein-parameters "with-profile +pretty repl :headless")
 '(custom-safe-themes
   (quote
    ("b181ea0cc32303da7f9227361bb051bbb6c3105bb4f386ca22a06db319b08882" "2b8dff32b9018d88e24044eb60d8f3829bd6bbeab754e70799b78593af1c3aba" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "de0b7245463d92cba3362ec9fe0142f54d2bf929f971a8cdf33c0bf995250bcf" "d61f6c49e5db58533d4543e33203fd1c41a316eddb0b18a44e0ce428da86ef98" "64ca5a1381fa96cb86fd6c6b4d75b66dc9c4e0fc1288ee7d914ab8d2638e23a9" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "1b27e3b3fce73b72725f3f7f040fd03081b576b1ce8bbdfcb0212920aec190ad" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(evil-cleverparens-use-additional-bindings nil)
 '(evil-cleverparens-use-additional-movement-keys nil)
 '(evil-cleverparens-use-regular-insert t)
 '(evil-search-module (quote evil-search))
 '(indent-tabs-mode nil)
 '(lsp-ui-doc-enable nil)
 '(package-selected-packages
   (quote
    (evil-goggles volatile-highlights volotile-highlights indent-tabs-mode rjsx-mode xref-js2 js2-refactor js2-mode dap-python dap origami rg treemacs-icons-dired treemacs-magit treemacs-magic treemacs-projectile editorconfig editorconfig-mode lsp-treemacs treemacs-evil lsp dap-java dap-mode lsp-java company-lsp lsp-ui lsp-mode use-package meghanada meghanada-mode flycheck-meghanada flycheck-java ggtags solarized-theme solarized-emacs flycheck-clj-kondo rust-mode flycheck-rust hindent yaml-mode web-mode xquery-mode tidy diminish airline-themes cider-repl wrap-region color-theme-solarized blackboard-theme zenburn-theme counsel org-bullets haskell-mode shakespeare-mode paxedit clj-refactor markdown-mode flycheck smex neotree evil-magit swiper flx-ido evil-surround cider evil-cleverparens lispy company evil-mc auto-complete evil-commentary evil-nerd-commenter multiple-cursors highlight-symbol badwolf-theme centered-cursor-mode helm-config drag-stuff evil-quickscope rainbow-delimiters swiper-helm helm-ag helg-ag magit projectile paredit evil-smartparens smartparens tabbar clojure-mode powerline linum-relative helm)))
 '(projectile-current-project-on-switch (quote keep))
 '(projectile-git-command "git ls-files -zco --exclude-standard ; echo -n todo.md")
 '(projectile-git-ignored-command "git ls-files -zcoi --exclude-standard | grep -v todo.md")
 '(tab-always-indent t)
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load "~/.emacs.d/init.el")
