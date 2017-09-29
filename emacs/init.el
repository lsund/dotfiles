;;; package --- Summary
;;; init file
(require 'package)

;;; Commentary:

;;; Code:

(package-refresh-contents)

(defun require-package (pkg)
  "Install package if it is not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (require pkg))

(defun require-packages (pkgs)
  (mapc 'require-package pkgs))

(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(server-start)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(require 'my-config)
;;; init.el ends here


;; Local Variables
;; byte-compile-warnings: (not suspicious)
;; End:
