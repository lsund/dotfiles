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

;; Clojure indent

(defun my-goto-end-of-form-rec (p)
  (let ((next-pos (paxedit-sexp-move-to-core-start)))
    (cond ((looking-at ".comment")
           (progn (message "commment")
                  (goto-char p)
                  (paredit-forward)))

          ((numberp next-pos)
           (my-goto-end-of-form-rec next-pos))

          (t
           (paredit-forward)))))

(defun my-goto-end-of-form ()
  (interactive)
  (my-goto-end-of-form-rec (point)))


(defun my-end-of-form ()
  (save-excursion
    (my-goto-end-of-form)
    (point)))

(defun my-beginning-of-form ()
  (save-excursion
    (my-goto-end-of-form)
    (paredit-backward)
    (point)))

(defun my-clojure-indent-defn ()
  (interactive)
  (save-excursion
    (goto-char (my-beginning-of-form))
    (indent-sexp)
    (clojure-align (point) (my-end-of-form))))

;; Resets
(define-key my-leader-map "n" nil)
(define-key my-leader-map "N" nil)

;; Insert
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)

;; normal
(define-key evil-normal-state-map (kbd "M-l") nil)
(define-key evil-normal-state-map (kbd "M-h") nil)
(define-key evil-normal-state-map (kbd "C-w h") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-w l") 'windmove-right)
(define-key evil-normal-state-map (kbd "C-w k") 'windmove-up)
(define-key evil-normal-state-map (kbd "C-w j") 'windmove-down)

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

(define-key my-leader-map "ii" 'my-clojure-indent-defn)

;; Windows
(define-key evil-normal-state-map (kbd "C-s -") 'split-window-below)
(define-key evil-normal-state-map (kbd "C-s |") 'split-window-right)
(define-key evil-normal-state-map (kbd "C-s w") 'delete-window)

(define-key my-leader-map "nh" 'remove-highlight)

;; Company company
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)

;; Counsel
(define-key my-leader-map "o" 'counsel-find-file)
(define-key my-leader-map "x" 'counsel-M-x)

;; Swiper swiper

(define-key my-leader-map "a" 'swiper)
(define-key my-leader-map "A" 'swiper-all)

;; Ivy

(define-key ivy-minibuffer-map (kbd "C-w") #'backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-h") #'backward-delete-char)
(define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-partial)

;; Projectile
(define-key my-leader-map "e" 'projectile-find-file)
(define-key my-leader-map "pp" 'projectile-switch-project)

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

;; Cleverparens

(setq evil-cleverparens-use-additional-bindings nil)


;; Cider

(define-key evil-normal-state-map (kbd "C-c r") 'cider-restart)
(define-key evil-normal-state-map (kbd "C-c j") 'cider-jack-in)
(define-key evil-normal-state-map (kbd "C-c f") 'cider-format-buffer)

;; Refresh

(define-key my-leader-map "r" 'cider-eval-buffer)

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

