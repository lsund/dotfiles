;; A very hacky solution. I don't even know why it works
(defun my-resize-window-left ()
  (interactive)
  (condition-case ex
      (if (message "%s" (windmove-left))
	  (progn
	    (shrink-window 8 t)
	    (windmove-right)))
    ('error
     (shrink-window 4 t))))

(defun my-resize-window-right ()
  (interactive)
  (condition-case ex
      (if (message "%s" (windmove-right))
	  (progn
	    (shrink-window 4 t)
	    (windmove-left)))
    ('error
     (shrink-window 4 t))))

(defun my-resize-window-up ()
  (interactive)
  (condition-case ex
      (if (message "%s" (windmove-up))
	  (progn
	    (shrink-window 4)
	    (windmove-down)))
    ('error
     (shrink-window 4))))

(defun my-resize-window-down ()
  (interactive)
  (condition-case ex
      (if (message "%s" (windmove-down))
	  (progn
	    (shrink-window 4)
	    (windmove-up)))
    ('error
     (shrink-window 4))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun archive-buffer ()
  (interactive)
  (kill-buffer)
  (if (get-buffer "*Buffer List*")
      (save-excursion
        (set-buffer "*Buffer List*")
        (revert-buffer))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Window  resizing
(define-key evil-normal-state-map (kbd "C-s j") 'my-resize-window-down)
(define-key evil-normal-state-map (kbd "C-s k") 'my-resize-window-up)
(define-key evil-normal-state-map (kbd "C-s h") 'my-resize-window-left)
(define-key evil-normal-state-map (kbd "C-s l") 'my-resize-window-right)

(define-key evil-normal-state-map (kbd "C-s <return>") 'new-frame)

;; Movement between windows
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

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

;; Buffer and Window management
(define-key my-leader-map "," 'save-buffer-always)
(define-key my-leader-map "<"  'save-some-buffers)
(define-key my-leader-map "'" 'kill-this-buffer)
(define-key my-leader-map "y" 'ido-switch-buffer)
(define-key my-leader-map "Y" 'switch-to-previous-buffer)
(define-key my-leader-map "-" 'split-window-below)
(define-key my-leader-map "|" 'split-window-right)
(define-key evil-normal-state-map (kbd "C-s w") 'delete-window)

;; Smex
(define-key my-leader-map "x" 'smex)
(define-key my-leader-map "X" 'smex-major-mode-commands)
(define-key evil-normal-state-map (kbd "M-X") 'execute-extended-command)

(define-key minibuffer-local-map (kbd "C-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-complete-history-element)

(provide 'my-bufferman)
