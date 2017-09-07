(provide 'my-keybindings)

;; Key bindings
(defvar my-leader-map (make-sparse-keymap))
(define-key evil-normal-state-map "\\" my-leader-map)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(defun scroll-down-and-center ()
  "Scrolls 5 lines down and centers the screen"
  (interactive)
  (forward-line 5)
  (recenter))

(defun scroll-up-and-center ()
  "Scrolls 5 lines up and centers the screen"
  (interactive)
  (forward-line -5)
  (recenter))

(defun push-line-down()
  "Inserts a new line and moves one line down"
  (interactive)
  (open-line 1)
  (forward-line 1))

(defun push-line-up()
  "Inserts a new line and moves one line down"
  (interactive)
  (kill-whole-line 1)
  (forward-line -1))

(defun remove-highlight()
  "Disable all kinds of code highlightning"
  (interactive)
  (highlight-symbol-remove-all)
  (evil-ex-nohighlight))

;; Insert
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)

;; Normal
(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)

(define-key evil-normal-state-map (kbd "K") 'scroll-up-and-center)
(define-key evil-normal-state-map (kbd "J") 'scroll-down-and-center)

(define-key evil-normal-state-map (kbd "C-j") 'push-line-down)
(define-key evil-normal-state-map (kbd "C-k") 'push-line-up)

(define-key evil-normal-state-map (kbd "C-f") 'scroll-up)
(define-key evil-normal-state-map (kbd "C-b") 'scroll-down)
(define-key evil-normal-state-map (kbd "C-u") 'scroll-half-page-down)
(define-key evil-normal-state-map (kbd "C-d") 'scroll-half-page-up)

(define-key my-leader-map "," 'save-buffer)
(define-key my-leader-map "<" (lambda () (interactive) (save-some-buffers t)))
(define-key my-leader-map "'" 'kill-this-buffer)
(define-key my-leader-map "\"" 'kill-other-buffers)
(define-key my-leader-map "y" 'switch-to-buffer)
(define-key my-leader-map "Y" 'switch-to-buffer)
(define-key my-leader-map "b" nil)

(define-key my-leader-map ";" 'eval-defun)

(define-key my-leader-map "x" 'helm-M-x)
(define-key my-leader-map "|" 'split-window-right)
(define-key my-leader-map "-" 'split-window-below)

(define-key my-leader-map "nh" 'remove-highlight)

;; Helm
(define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)
(define-key my-leader-map "o" 'helm-find-files)

;; Company
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)

;; Projectile
(define-key my-leader-map "e" 'helm-projectile-find-file)

;; Drag stuff
(define-key evil-normal-state-map (kbd "C-S-j") 'drag-stuff-down)
(define-key evil-normal-state-map (kbd "C-S-k") 'drag-stuff-up)
(define-key evil-normal-state-map (kbd "C-S-l") 'drag-stuff-right)
(define-key evil-normal-state-map (kbd "C-S-h") 'drag-stuff-left)

(define-key evil-visual-state-map (kbd "C-S-j") 'drag-stuff-down)
(define-key evil-visual-state-map (kbd "C-S-k") 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "C-S-l") 'drag-stuff-right)
(define-key evil-visual-state-map (kbd "C-S-h") 'drag-stuff-left)

;; Highlight Symbol

(define-key my-leader-map "h" 'highlight-symbol)
(define-key my-leader-map "j" 'highlight-symbol-next)
(define-key my-leader-map "k" 'highlight-symbol-prev)

;; Refresh clojure
(defun util-clojure/cider-cmd (cmd)
  (let ((cmd cmd))
    (lambda ()
      (interactive)
      (cider-interactive-eval cmd))))

;; (define-key my-leader-map "r"
;;   (lambda ()
;;     (interactive)
;;     (cider-interactive-eval
;;      "(do (de.doctronic.sqldump-manager.main/stop-server) (require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh-all) (de.doctronic.sqldump-manager.main/start-server))")))

(define-key my-leader-map "r"
  (lambda ()
    (interactive)
    (cider-interactive-eval
     "(user/system-restart!)")))
