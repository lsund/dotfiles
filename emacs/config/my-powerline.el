(defun powerline-my-theme ()
  "My custom powerline theme."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
	     (mode-line-buffer-id
	      (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
	     (mode-line (if active 'mode-line 'mode-line-inactive))
	     (face1 (if active 'powerline-active1 'powerline-inactive1))
	     (face2 (if active 'powerline-active2 'powerline-inactive2))
	     (separator-left (intern (format
				      "powerline-%s-%s"
				      (powerline-current-separator)
				      (car powerline-default-separator-dir))))
	     (separator-right (intern
			       (format "powerline-%s-%s"
				       (powerline-current-separator)
				       (cdr powerline-default-separator-dir))))
	     (lhs (list (powerline-raw "%*" mode-line 'l)
			;; Buffer size
			(when powerline-display-buffer-size
			  (powerline-buffer-size mode-line 'l))
			;; Module info
			(when powerline-display-mule-info
			  (powerline-raw mode-line-mule-info mode-line 'l))
			(powerline-buffer-id mode-line-buffer-id 'l)))
	     (rhs (list (powerline-raw global-mode-string face2 'r)
			(funcall separator-right face2 face1)
			(unless window-system
			  (powerline-raw (char-to-string #xe0a1) face1 'l))
			(powerline-raw "%4l" face1 'l)
			(powerline-raw ":" face1 'l)
			(powerline-raw "%3c" face1 'r)
			(funcall separator-right face1 mode-line)
			(powerline-raw " ")
			(powerline-raw "%6p" mode-line 'r)
			(when powerline-display-hud
			  (powerline-hud face2 face1)))))
	(concat (powerline-render lhs)
		(powerline-fill face2 (powerline-width rhs))
		(powerline-render rhs)))))))

(provide 'my-powerline)
(powerline-my-theme)
