;; Use M-x ghcid to launch

;; Set ghcid-target to change the stack target
(setq ghcid-target "")

(setq ghcid-height 100)

(defun ghcid-stack-cmd (target)
      (format "stack ghci %s --test --bench --ghci-options=-fno-code" target))

(setq ghcid-buf-name "*ghcid*")

(add-hook 'term-mode-hook
          (lambda ()
            (interactive)
            (term-line-mode)
            (define-key evil-insert-state-map (kbd "C-p") 'term-previous-input)
            (define-key evil-insert-state-map (kbd "C-n") 'term-next-input)
            (define-key term-mode-map (kbd "C-p") 'term-previous-input)
            (define-key term-mode-map (kbd "C-n") 'term-next-input)
            (define-key term-mode-map (kbd "C-u") 'term-kill-input)
            (define-key term-mode-map (kbd "C-b") 'left-char)
            (define-key term-mode-map (kbd "C-f") 'right-char)
            (define-key term-mode-map (kbd "C-d") 'term-send-eof)
            (define-key evil-insert-state-map (kbd "C-d") 'term-send-eof)
            (define-key evil-insert-state-map (kbd "C-a") 'term-send-left)
            (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
            ))

(define-minor-mode ghcid-mode
  "A minor mode for ghcid terminals"
  :lighter " Ghcid"
  (nlinum-mode -1)
  (linum-mode -1)
  (compilation-minor-mode))

(defun new-ghcid-term ()
  (interactive)
  (kill-ghcid)
  (let ((ghcid-buf (get-buffer-create ghcid-buf-name)))
    (display-buffer
     ghcid-buf
     '((display-buffer-at-bottom
        display-buffer-pop-up-window
        display-buffer-reuse-window)
       (window-height . 15)))
    (select-window (get-buffer-window ghcid-buf))
    (make-term "ghcid" "/bin/zsh")
    (term-mode)
    ;; (term-char-mode)
    ;; (term-set-escape-char ?\C-x)
    (setq-local term-buffer-maximum-size ghcid-height)
    (setq-local scroll-up-ggressively 1)
    (ghcid-mode)))

(defun kill-ghcid ()
  (let* ((ghcid-buf (get-buffer ghcid-buf-name))
         (ghcid-proc (get-buffer-process ghcid-buf)))
    (when (processp ghcid-proc)
      (progn
        (set-process-query-on-exit-flag ghcid-proc nil)
        (kill-process ghcid-proc)))))

(defun add-stars (s) (format "*%s*" s))

;; TODO Pass in compilation command like compilation-mode
(defun ghcid-command (h)
    (format "ghcid -c \"%s\" -h %s\n" (ghcid-stack-cmd ghcid-target) h))

;; TODO Close stuff if it fails
(defun ghcid ()
  "Run ghcid"
  (interactive)
  (let ((cur (selected-window)))
    (new-ghcid-term)
    (comint-send-string ghcid-buf-name (ghcid-command ghcid-height))
    (select-window cur)))

;; Assumes that only one window is open
(defun ghcid-stop ()
  "Stop ghcid"
  (interactive)
  (let* ((ghcid-buf (get-buffer ghcid-buf-name))
         (ghcid-window (get-buffer-window ghcid-buf)))
    (when ghcid-buf
      (progn
        (kill-ghcid)
        (select-window ghcid-window)
        (kill-buffer-and-window)))))

(provide 'my-ghcid)
