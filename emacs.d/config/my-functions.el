;; package --- Summary
;;; Top level config file

;;; Commentary:

;;; Code:

(defun slurp (f)
  "Read the contents of a file F."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun iwb ()
  "Indent the whole buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun push-line-down()
  "Insert a new line and move one line down."
  (interactive)
  (progn
    (back-to-indentation)
    (evil-insert-state)
    (evil-ret-and-indent)
    (indent-for-tab-command)
    (evil-normal-state)))

(defun push-line-up()
  "Insert a new line and move one line up."
  (interactive)
  (progn
    (back-to-indentation)
    (forward-line -1)
    (kill-whole-line 1)
    (evil-forward-word-begin)))

(defun remove-highlight()
  "Disable all kinds of code highlightning."
  (interactive)
  (highlight-symbol-remove-all)
  (evil-ex-nohighlight))

(evil-define-motion myevil-next-line (count)
  :type exclusive
  (evil-line-move (or count 5))
  (recenter))

(evil-define-motion myevil-prev-line (count)
  "Move the cursor COUNT screen lines down, or 5."
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

(defun scroll-half-page-down ()
  "Scroll down half the page."
  (interactive)
  :keep-visual t
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "Scroll up half the page."
  (interactive)
  :keep-visual t
  (scroll-up (/ (window-body-height) 2)))

(defun noop ()
  "Do nothing."
  (interactive))

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

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

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

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(defun my-imenu-refresh()
  (interactive)
  (lsp-ui-imenu)
  (windmove-left))

(defun my-imenu-toggle()
  (interactive)
  (if (get-buffer "*lsp-ui-imenu*")
      (progn
        (windmove-right)
        (kill-this-buffer))
    (progn
      (lsp-ui-imenu)
      (windmove-left))))

(defun json-format ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(provide 'my-functions)
;;; my-functions.el ends here
