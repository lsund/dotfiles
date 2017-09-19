(require 'my-clojure)
(require 'my-elisp)

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
  :keep-visual t
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  :keep-visual t
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(defun push-line-down()
  "Inserts a new line and moves one line down"
  (interactive)
  (back-to-indentation)
  (open-line 1)
  (forward-line 1))

(defun push-line-up()
  "Inserts a new line and moves one line down"
  (interactive)
  (back-to-indentation)
  (forward-line -1)
  (kill-whole-line 1))

(defun remove-highlight()
  "Disable all kinds of code highlightning"
  (interactive)
  (highlight-symbol-remove-all)
  (evil-ex-nohighlight))

;; Resets
(define-key my-leader-map "n" nil)
(define-key my-leader-map "N" nil)


; Insert
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)

;; normal
(define-key evil-normal-state-map (kbd "C-w h") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-w l") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-w k") 'windmove-up)
(define-key evil-normal-state-map (kbd "C-w j") 'windmove-down)

(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)

(define-key evil-insert-state-map (kbd "M-h") 'windmove-left)
(define-key evil-insert-state-map (kbd "M-l") 'windmove-right)
(define-key evil-insert-state-map (kbd "M-k") 'windmove-up)
(define-key evil-insert-state-map (kbd "M-j") 'windmove-down)

(define-key evil-normal-state-map (kbd "C-j") 'push-line-down)
(define-key evil-normal-state-map (kbd "C-k") 'push-line-up)

(evil-define-motion myevil-next-line (count)
  :type exclusive
  (evil-line-move (or count 5))
  (recenter))

(evil-define-motion myevil-prev-line (count)
  :type exclusive
  (evil-line-move (or count -5))
  (recenter))

(evil-define-motion myevil-next-visual-line (count)
  "Move the cursor COUNT screen lines down, or 5."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 5))
    (recenter)))

(evil-define-motion myevil-prev-visual-line (count)
"Move the cursor COUNT screen lines down, or 5."
:type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count -5))
    (recenter)))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(define-key evil-visual-state-map (kbd "J") 'myevil-next-visual-line)
(define-key evil-visual-state-map (kbd "K") 'myevil-prev-visual-line)
(define-key evil-normal-state-map (kbd "J") 'myevil-next-line)
(define-key evil-normal-state-map (kbd "K") 'myevil-prev-line)

(define-key evil-normal-state-map (kbd "C-f") 'scroll-up)
(define-key evil-normal-state-map (kbd "C-b") 'scroll-down)
(define-key evil-normal-state-map (kbd "C-u") 'scroll-half-page-down)
(define-key evil-normal-state-map (kbd "C-d") 'scroll-half-page-up)

(define-key my-leader-map "," 'save-buffer)
(define-key my-leader-map "<" (lambda () (interactive) (save-some-buffers t)))
(define-key my-leader-map "'" 'kill-this-buffer)
(define-key my-leader-map "\"" 'kill-other-buffers)
(define-key my-leader-map "y" 'switch-to-buffer)
(define-key my-leader-map "Y" 'switch-to-previous-buffer)
(define-key my-leader-map "b" nil)

;; (define-key my-leader-map "ii" 'my-clojure-indent-defn)

(define-key evil-normal-state-map (kbd "C-s z") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "C-s Z") 'winner-undo)

;; Windows
(define-key evil-normal-state-map (kbd "C-s -") 'split-window-below)
(define-key evil-normal-state-map (kbd "C-s |") 'split-window-right)
(define-key evil-normal-state-map (kbd "C-s w") 'delete-window)

(define-key my-leader-map "nh" 'remove-highlight)

;; Company company
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)

(define-key evil-insert-state-map (kbd "<backtab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)

;; Counsel
(define-key my-leader-map "o" 'counsel-find-file)
(define-key my-leader-map "x" 'execute-extended-command)
(define-key my-leader-map "a" 'counsel-ag)

;; Ivy

(defun my-ivy ()
  (interactive)
  (if (ivy-partial)
      nil
    (ivy-next-line)))

(defun my-ivy-back ()
  (interactive)
  (if (ivy-partial)
      nil
    (ivy-previous-line)))

(define-key ivy-minibuffer-map (kbd "C-w") #'backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-h") #'backward-delete-char)
(define-key ivy-minibuffer-map (kbd "<tab>") 'my-ivy)
(define-key ivy-minibuffer-map (kbd "<backtab>") 'my-ivy-back)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; Projectile
(define-key my-leader-map "e" 'counsel-projectile)
(define-key my-leader-map "pp" 'counsel-projectile-switch-project)

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

;; Refresh

(define-key my-leader-map "R"
  (lambda ()
    (interactive)
    (cider-interactive-eval
     "(user/system-restart!)")))

(define-key my-leader-map "v"
  (lambda ()
    (interactive)
    (cider-interactive-eval
     "(do (require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh-all))")))

;; Neotree

(define-key my-leader-map "nn" 'neotree-toggle)
(define-key my-leader-map "NN" 'neotree-find)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)

;; Magit magit

(define-key my-leader-map (kbd "gs") 'magit-status)

;; Archive buffer

(defun archive-buffer ()
  (interactive)
  (kill-buffer)
  (if (get-buffer "*Buffer List*")
      (save-excursion
        (set-buffer "*Buffer List*")
        (revert-buffer))))

;;; my-keybindings.el ends here

(provide 'my-keybindings)
