;;; package --- Summary


;; Scala specific config

;;; Commentary:

;;; Code:

(provide 'my-scala)

(require-packages '(ensime scala-mode))

(add-to-list 'exec-path "/usr/local/bin")

(eval-after-load "scala-mode"
  '(progn
     (evil-define-key 'normal ensime-mode-map
       (kbd "C-c j") 'ensime-inf-switch)))

;;; my-scala.el ends here
