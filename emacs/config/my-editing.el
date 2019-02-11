(defun push-line-down()
  "Inserts a new line and moves one line down"
  (interactive)
  (back-to-indentation)
  (newline)
  (indent-for-tab-command))

(defun push-line-up()
  "Inserts a new line and moves one line down"
  (interactive)
  (back-to-indentation)
  (forward-line -1)
  (kill-whole-line 1)
  (evil-forward-word-begin))

(defun remove-highlight()
  "Disable all kinds of code highlightning"
  (interactive)
  (highlight-symbol-remove-all)
  (evil-ex-nohighlight))

(defun my-kill-word ()
  "Kill a word or a portion of whitespace. If the thing behind the cursor is a
   letter, kill the whole word. If it's a tab, kill the single tab. If its whitespace, delete 4 spaces or until the next letter."
  (interactive)
  (cond
   ((string= (string (preceding-char)) "\t") (delete-backward-char 1))
   ((string= (string (preceding-char)) " ") (delete-backward-until-letter 4))
   (t (backward-kill-word 1))))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

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

(defun scroll-half-page-down ()
  "Scroll down half the page."
  :keep-visual t
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "Scroll up half the page."
  :keep-visual t
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(defun noop ()
  "do Nothing"
  (interactive))

;; Navigation
(define-key evil-visual-state-map (kbd "J") 'myevil-next-visual-line)
(define-key evil-visual-state-map (kbd "K") 'myevil-prev-visual-line)
(define-key evil-normal-state-map (kbd "J") 'myevil-next-line)
(define-key evil-normal-state-map (kbd "K") 'myevil-prev-line)
(define-key evil-normal-state-map (kbd "C-f") 'scroll-up)
(define-key evil-normal-state-map (kbd "C-b") 'scroll-down)
(define-key evil-normal-state-map (kbd "C-u") 'scroll-half-page-down)
(define-key evil-normal-state-map (kbd "C-d") 'scroll-half-page-up)

;; Editing
(define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-j") 'push-line-down)
(define-key evil-normal-state-map (kbd "C-k") 'push-line-up)
(define-key evil-normal-state-map (kbd "Y")
  (lambda ()
    (interactive)
    (execute-kbd-macro (read-kbd-macro "y $"))))
(define-key evil-insert-state-map (kbd "<backspace>") #'noop)

;; Indentation

(define-key my-leader-map "bd" 'iwb)

;; Search
(define-key my-leader-map "nh" 'remove-highlight)
(define-key evil-normal-state-map (kbd "/") 'swiper)
(define-key evil-normal-state-map (kbd "M-n" ) 'highlight-symbol-next)
(define-key evil-normal-state-map (kbd "M-p" ) 'highlight-symbol-prev)
(define-key evil-normal-state-map (kbd "*") (lambda ()
                                              (interactive)
                                              (remove-highlight)
                                              (highlight-symbol)))

;; Drag stuff
(define-key evil-normal-state-map (kbd "C-S-k") 'drag-stuff-up)
(define-key evil-normal-state-map (kbd "C-S-j") 'drag-stuff-down)
(define-key evil-normal-state-map (kbd "C-S-l") 'drag-stuff-right)
(define-key evil-normal-state-map (kbd "C-S-h") 'drag-stuff-left)
(define-key evil-visual-state-map (kbd "C-S-j") 'drag-stuff-down)
(define-key evil-visual-state-map (kbd "C-S-k") 'drag-stuff-up)
(define-key evil-visual-state-map (kbd "C-S-l") 'drag-stuff-right)
(define-key evil-visual-state-map (kbd "C-S-h") 'drag-stuff-left)

;; Company
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") 'delete-backward-char)
(define-key company-active-map (kbd "C-w") 'backward-kill-word)
(define-key company-active-map (kbd "RET") 'company-abort)
(define-key evil-insert-state-map (kbd "<backtab>")
  'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "<backtab>") 'company-select-previous)

(provide 'my-editing)
