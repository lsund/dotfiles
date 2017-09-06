(provide 'my-config)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
        (delq (current-buffer) 
              (remove-if-not 'buffer-file-name (buffer-list)))))

(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)

(define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "M-l") 'windmove-right)
(define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "M-j") 'windmove-down)

(defvar my-leader-map (make-sparse-keymap))

(define-key evil-normal-state-map "\\" my-leader-map)

(define-key my-leader-map "," 'save-buffer)
(define-key my-leader-map "q" 'kill-this-buffer)
(define-key my-leader-map "Q" 'kill-other-buffers)

(define-key my-leader-map "'" 'eval-defun)

(define-key my-leader-map "b" 'helm-buffers-list)
(define-key my-leader-map "e" 'helm-find-files)

(define-key my-leader-map "o" 'find-file)

(define-key my-leader-map "x" 'helm-M-x)
(define-key my-leader-map "|" 'split-window-right)

(define-key my-leader-map "Y" 'tabbar-backward)
(define-key my-leader-map "y" 'tabbar-forward)

(define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)

(require-packages '(helm
		    linum-relative
		    powerline
		    clojure-mode
		    aggressive-indent
		    tabbar
		    smartparens
		    evil-quickscope

		    helm-ag
		    paredit
		    projectile
		    magit
		    swiper-helm
		    rainbow-delimiters))

(require 'helm-config)
(helm-mode 1)
(tabbar-mode 1)
(smartparens-mode 1)
(global-aggressive-indent-mode 1)
(rainbow-delimiters-mode 1)
(evil-quickscope-always-mode)

(linum-mode)
(linum-relative-global-mode)
(setq column-number-mode t)

(powerline-vim-theme)

(menu-bar-mode 1)
(tool-bar-mode -1)

