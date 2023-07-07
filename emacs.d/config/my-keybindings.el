;; package --- Summary
;;; Top level config file

;;; Commentary:

;;; Code:

;; New Leader map
(defvar my-leader-map (make-sparse-keymap))
(define-key evil-normal-state-map "\\" my-leader-map)
(define-key my-leader-map "n" nil)
(define-key my-leader-map "N" nil)

(global-set-key [C-f1] 'show-file-name)
(global-set-key [C-f2] 'test)

;; Navigate Buffer
(define-key evil-visual-state-map (kbd "J") 'myevil-next-visual-line)
(define-key evil-visual-state-map (kbd "K") 'myevil-prev-visual-line)
(define-key evil-normal-state-map (kbd "J") 'myevil-next-line)
(define-key evil-normal-state-map (kbd "K") 'myevil-prev-line)
(define-key evil-normal-state-map (kbd "C-f") 'scroll-up)
(define-key evil-normal-state-map (kbd "C-b") 'scroll-down)
(define-key evil-normal-state-map (kbd "C-u") 'scroll-half-page-down)
(define-key evil-normal-state-map (kbd "C-d") 'scroll-half-page-up)

;; Navigate Project
(define-key my-leader-map "e" 'projectile-find-file)
(define-key my-leader-map "j" 'projectile-switch-project)
(define-key my-leader-map "o" 'ido-find-file)
(define-key my-leader-map "nn" 'treemacs)
(define-key my-leader-map "na" 'treemacs-add-and-display-current-project)
(define-key my-leader-map "nt" 'my-imenu-toggle)
(define-key my-leader-map "nr" 'my-imenu-refresh)
(define-key my-leader-map "nd" 'treemacs-remove-project-from-workspace)
(define-key treemacs-mode-map (kbd "M-l") 'windmove-right)
(define-key treemacs-mode-map (kbd "M-h") 'windmove-left)
(define-key treemacs-mode-map (kbd "M-h") 'windmove-left)
(define-key evil-treemacs-state-map (kbd "J") 'myevil-next-line)
(define-key evil-treemacs-state-map (kbd "K") 'myevil-prev-line)
(define-key evil-treemacs-state-map  (kbd "v") 'treemacs-peek)
(define-key evil-motion-state-map (kbd "\\") nil)
(add-hook 'treemacs-mode-hook
          (lambda ()
            (interactive)
            (defvar treemacs-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))
            (define-key treemacs-mode-map "\\" treemacs-leader-map)

            ;; Insert separator
            (define-key treemacs-leader-map "e" 'projectile-find-file)))
;; Editing
(define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)
(define-key ido-file-completion-map (kbd "C-w") 'backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-w") #'backward-kill-word)
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)
(define-key ivy-minibuffer-map (kbd "C-h") #'backward-delete-char)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-j") 'push-line-down)
(define-key evil-normal-state-map (kbd "C-k") 'push-line-up)
(define-key evil-normal-state-map (kbd "Y")
  (lambda ()
    (interactive)
    (execute-kbd-macro (read-kbd-macro "y $"))))
(define-key evil-insert-state-map (kbd "<backspace>") #'noop)
(define-key evil-normal-state-map (kbd "C-S-k") 'drag-stuff-up)
(define-key evil-normal-state-map (kbd "C-S-j") 'drag-stuff-down)
(define-key evil-normal-state-map (kbd "C-S-l") 'drag-stuff-right)
(define-key evil-normal-state-map (kbd "C-S-h") 'drag-stuff-left)
(define-key evil-visual-state-map (kbd "C-S-j") 'drag-stuff-down)
(define-key evil-visual-state-map (kbd "C-S-k") 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "C-S-l") 'drag-stuff-right)
(define-key evil-visual-state-map (kbd "C-S-h") 'drag-stuff-left)
(define-key minibuffer-local-map (kbd "C-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-complete-history-element)
(define-key ido-file-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-file-completion-map (kbd "C-p") 'ido-prev-match)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; Autocompletion
(define-key company-active-map (kbd "RET") 'company-abort)
(define-key evil-insert-state-map (kbd "<backtab>")
  'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key ivy-minibuffer-map (kbd "<tab>") 'my-ivy)
(define-key ivy-minibuffer-map (kbd "<backtab>") 'my-ivy-back)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; Window Management
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
(define-key evil-normal-state-map (kbd "C-s <return>") 'new-frame)
(define-key evil-normal-state-map (kbd "C-s j") 'my-resize-window-down)
(define-key evil-normal-state-map (kbd "C-s k") 'my-resize-window-up)
(define-key evil-normal-state-map (kbd "C-s h") 'my-resize-window-left)
(define-key evil-normal-state-map (kbd "C-s l") 'my-resize-window-right)
(define-key my-leader-map "," 'save-buffer-always)
(define-key my-leader-map "<"  'save-some-buffers)
(define-key my-leader-map "'" 'kill-this-buffer)
(define-key my-leader-map "y" 'ido-switch-buffer)
(define-key my-leader-map "Y" 'switch-to-previous-buffer)
(define-key my-leader-map "-" 'split-window-below)
(define-key my-leader-map "|" 'split-window-right)
(define-key evil-normal-state-map (kbd "C-s w") 'delete-window)

;; Run commands
(define-key my-leader-map "x" 'smex)
(define-key my-leader-map "X" 'smex-major-mode-commands)
(define-key evil-normal-state-map (kbd "M-X") 'execute-extended-command)

;; Indentation
(define-key my-leader-map "bd" 'iwb)
(define-key evil-insert-state-map (kbd "<tab>") 'tab-to-tab-stop)
(setq tab-stop-list (number-sequence 4 120 4))

;; Search
(define-key my-leader-map "nh" 'remove-highlight)
(define-key evil-normal-state-map (kbd "/") 'swiper)
(define-key evil-normal-state-map (kbd "M-n" ) 'highlight-symbol-next)
(define-key evil-normal-state-map (kbd "M-p" ) 'highlight-symbol-prev)
(define-key my-leader-map "ar" (lambda ()
                                 (interactive)
                                 (ivy-resume)))
(define-key my-leader-map "aa" (lambda ()
                                 (interactive)
                                 (counsel-ag "")))
(define-key my-leader-map "aA" (lambda ()
                                 (interactive)
                                 (counsel-ag "" nil "-w")))
(define-key my-leader-map "ak" (lambda ()
                                 (interactive)
                                 (counsel-ag (thing-at-point 'symbol))))
(define-key my-leader-map "aK" (lambda ()
                                 (interactive)
                                 (counsel-ag (thing-at-point 'symbol) nil "-w")))
;; Align after regex
(define-key evil-visual-state-map (kbd "M-a") 'align-regexp)

;; Magit
(define-key evil-normal-state-map (kbd "gs") 'magit-status)
(define-key magit-mode-map "\\" my-leader-map)
(define-key evil-magit-toggle-text-minor-mode-map "\\" my-leader-map)
(evil-define-key evil-magit-state magit-mode-map
  "\\" my-leader-map)
(define-key evil-normal-state-map (kbd "gbl") 'magit-blame-addition)

;; Change font size
(define-key evil-normal-state-map (kbd "C-+") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-normal-state-map (kbd "C-0")
  (lambda () (interactive) (text-scale-increase 0)))


(provide 'my-keybindings)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; my-keybindings.el ends here
