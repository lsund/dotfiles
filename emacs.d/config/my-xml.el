;;; package --- Summary

;;; Commentary:

;; My Emacs config for editing XML

;;; Code:

(provide 'my-xml)

(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))



(add-hook 'nxml-mode-hook 'hs-minor-mode)

;; optional key bindings, easier than hs defaults
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(defun delete-tag ()
  (interactive)
  (save-excursion
    (sgml-skip-tag-backward 1)
    (delete-region (point)
                   (progn
                     (sgml-skip-tag-forward 1)
                     (point)))))

(defun copy-tag ()
  (interactive)
  (save-excursion
    (sgml-skip-tag-backward 1)
    (copy-region-as-kill (point)
                         (progn
                           (sgml-skip-tag-forward 1)
                           (point)))))

(defun insert-tag (name)
  "Insert the tag <NAME></NAME> and move the cursor to the middle of the tag."
  (interactive "sEnter tag name: ")
  (insert (format "<%s></%s>" name name))
  ;; 60 is the '<' character
  (evil-find-char-backward 1 60))

(defun insert-tag-newline (name)
  "TODO"
  (interactive "sEnter tag name: ")
  (insert-tag name)
  (open-line 1)
  (newline)
  (tab-to-tab-stop))

(defun sgml-copy-tagged-text ()
  (interactive)
  (save-excursion
    (search-backward ">")
    (evil-forward-char 1 1)
    (copy-region-as-kill (point)
                         (progn
                           (search-forward "<")
                           (evil-backward-char 1)
                           (point)))))

(defun sgml-delete-tagged-text ()
  "delete text between the tags that contain the current point"
  (interactive)
  (sgml-copy-tagged-text)
  (let ((b (point)))
    (sgml-skip-tag-backward 1)
    (when (not (eq b (point)))
      ;; moved somewhere, should be at front of a tag now
      (save-excursion
        (forward-sexp 1)
        (setq b (point)))
      (sgml-skip-tag-forward 1)
      (backward-sexp 1)
      (delete-region b (point)))))


(defun sgml-wrap-tagged-text ()
  (interactive)
  (search-backward ">")
  (evil-forward-char 1 1)
  (evil-visual-char (point))
  (search-forward "<")
  (evil-backward-char 1)
  (wrap-region-with-tag))

(defun my-xml-format ()
  "Format an XML buffer with `xmllint'."
  (interactive)
  (setq tmp-point (point))
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -format -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t)
  (goto-char tmp-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lambdawerk

;; bubak / Dean Allred / Tim Helmstedt, 2010-2015, http://stackoverflow.com/a/4280824
;; 20160906, mgr: added inhibit-read-only to use in cider-any-uruk
(defun nxml-pretty-format ()
    (interactive)
    (let ((inhibit-read-only t))
      (save-excursion
        (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
        (nxml-mode)
        (indent-region 0 (count-lines (point-min) (point-max))))))


(add-hook 'nxml-mode-hook
          (lambda ()
            (interactive)

            ;; Leader map extension
            (defvar xml-leader-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map my-leader-map)
                map))

            (evil-define-key 'normal nxml-mode-map "\\" xml-leader-map)

            (define-key evil-insert-state-map (kbd "TAB") 'completion-at-point)

            (define-key evil-visual-state-map (kbd "C-c i")
              (lambda ()
                (interactive)
                (wrap-region-with-tag)
                (evil-force-normal-state)))
            (define-key evil-normal-state-map (kbd "C-c c") 'copy-tag)
            (define-key evil-normal-state-map (kbd "C-c d") 'delete-tag)
            (define-key evil-normal-state-map (kbd "C-c i") 'insert-tag)
            (define-key evil-insert-state-map (kbd "C-c i") 'insert-tag)
            (define-key evil-normal-state-map (kbd "C-c I") 'insert-tag-newline)
            (define-key evil-insert-state-map (kbd "C-c I") 'insert-tag-newline)
            (define-key evil-normal-state-map (kbd "M-r") 'sgml-delete-tag)
            (define-key evil-normal-state-map (kbd "M-d") 'sgml-delete-tagged-text)
            (define-key evil-normal-state-map (kbd "M-y") 'sgml-copy-tagged-text)
            (define-key evil-normal-state-map (kbd "M-w") 'sgml-wrap-tagged-text)
            (define-key evil-normal-state-map (kbd "M-c")
              (lambda ()
                (interactive)
                (sgml-delete-tagged-text)
                (evil-insert-state)))
            (define-key xml-leader-map "bd"
              (lambda ()
                (interactive)
                (save-excursion
                  (iwb))))

            (set-fill-column 110)
            (auto-fill-mode 1)
            (wrap-region-mode t)))

(defun my-sgml-insert-gt ()
  "Inserts a `>' character and calls
`my-sgml-close-tag-if-necessary', leaving point where it is."
  (interactive)
  (insert ">")
  (save-excursion (my-sgml-close-tag-if-necessary)))

(defun my-sgml-close-tag-if-necessary ()
  "Calls sgml-close-tag if the tag immediately before point is
an opening tag that is not followed by a matching closing tag."
  (when (looking-back "<\\s-*\\([^</> \t\r\n]+\\)[^</>]*>")
    (let ((tag (match-string 1)))
      (unless (and (not (sgml-unclosed-tag-p tag))
                   (looking-at (concat "\\s-*<\\s-*/\\s-*" tag "\\s-*>")))
        (sgml-close-tag)))))

(eval-after-load "sgml-mode"
  '(define-key sgml-mode-map ">" 'my-sgml-insert-gt))

(eval-after-load "web-mode"
  '(setq web-mode-tag-auto-close-style 2))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-xml.el ends here
