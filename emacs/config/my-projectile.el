;; package --- Summary
;;; Top level config file

;;; Commentary:

;;; Code:

(require-packages '(projectile))

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
;; Caching could greatly speed up file navigation on big projects
(setq projectile-enable-caching nil)
(setq projectile-require-project-root nil)

(setq projectile-globally-ignored-directories
      (append
       '(".git"
         ".svn"
         "out"
         "repl"
         "target"
         "venv")
       projectile-globally-ignored-directories))

(setq projectile-globally-ignored-files
      (append
       '(".DS_Store"
         "*.gz"
         "*.pyc"
         "*.jar"
         "*.tar.gz"
         "*.tgz"
         "*.zip")
       projectile-globally-ignored-files))
(projectile-global-mode)

;; Projectile
(define-key my-leader-map "**" 'projectile-find-file)
(define-key my-leader-map "j" 'projectile-switch-project)

(provide 'my-projectile)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-config.el ends here
