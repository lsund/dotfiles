;;; package --- Summary
;;; init file
(require 'package)

;;; Commentary:

;;; Code:

(package-refresh-contents)

(defun require-package (pkg)
  "Install package if it is not already installed.
PKG: the package"
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (require pkg))

(defun require-packages (pkgs)
  "Require a list of packages if not already installed.
PKGS: The list of packages."
  (mapc 'require-package pkgs))

(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
		  ("melpa-stable" . "https://stable.melpa.org/packages/")))

  (add-to-list 'package-archives source t))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(server-start)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path "/home/lsund/Documents/dotfiles/emacs/config")
(add-to-list 'load-path "/home/lsund/Documents/dotfiles/emacs")

(require 'my-config)

;; Local Variables
;; byte-compile-warnings: (not suspicious)
;; End:

;;; init.el ends here
