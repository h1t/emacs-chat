(defface pidgin-chat-activity-face
  '((t (:foreground "firebrick" :weight bold)))
  "face for activity message"
  :group 'pidgin-chat)

(defvar pidgin-activity-mode-string "")

(defvar pidgin-activity-list nil)

(put 'pidgin-activity-mode-string 'risky-local-variable t)

(defun pidgin-activity-show-p (from)
  (let ((buffer (get-buffer (pidgin-chat-get-buffer from))))
         (get-buffer-window buffer 'visible)))

(defun pidgin-activity-add (from)
  (unless (pidgin-activity-show-p from)
    (add-to-list 'pidgin-activity-list from)
    (pidgin-activity-mode-line-update)))

(defun pidgin-activity-clean ()
  (when pidgin-activity-list
    (setq pidgin-activity-list
          (pidgin-delete-if 'pidgin-activity-show-p pidgin-activity-list))
    (pidgin-activity-mode-line-update)))


(defun pidgin-activity-switch-to (user)
  (interactive)
  (switch-to-buffer (pidgin-chat-get-buffer user))
  (pidgin-activity-clean))


(defun pidgin-activity-mode-line-update ()
  (setq pidgin-activity-mode-string
        (if pidgin-activity-list
            (concat "----"
                    (mapconcat
                     (lambda (x)
                       (propertize
                        x
                        'face 'pidgin-chat-activity-face
                        'local-map (make-mode-line-mouse-map
                                    'mouse-1 `(lambda ()
                                                (interactive)
                                                (pidgin-activity-switch-to ,x)))
                        'help-echo (concat "Jump to " x  "'s buffer")))
                     pidgin-activity-list ","))
        ""))
  (force-mode-line-update 'all))


;;;###autoload
(define-minor-mode pidgin-activity-mode
  :global t
  :init-value t
  (if pidgin-activity-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  'pidgin-activity-clean)
        (setq global-mode-string (append global-mode-string
                                         (list '(t pidgin-activity-mode-string)))))
    (progn
      (remove-hook 'window-configuration-change-hook
                     'pidgin-activity-clean)
      (setq global-mode-string (delete '(t pidgin-activity-mode-string)
                                       global-mode-string)))))


(if pidgin-activity-mode (pidgin-activity-mode 1))
  

(provide 'pidgin-chatactivity)