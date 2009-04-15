(defface shem-chat-activity-face
  '((t (:foreground "firebrick" :weight bold)))
  "face for activity message"
  :group 'shem-chat)

(defvar shem-activity-mode-string "")

(defvar shem-activity-list nil)

(put 'shem-activity-mode-string 'risky-local-variable t)

(defun shem-activity-show-p (from)
  (let ((buffer (get-buffer (shem-chat-get-buffer from))))
         (get-buffer-window buffer 'visible)))

(defun shem-activity-add (from)
  (unless (shem-activity-show-p from)
    (add-to-list 'shem-activity-list from)
    (shem-activity-mode-line-update)))

(defun shem-activity-clean ()
  (when shem-activity-list
    (setq shem-activity-list
          (shem-delete-if 'shem-activity-show-p shem-activity-list))
    (shem-activity-mode-line-update)))


(defun shem-activity-switch-to (user)
  (interactive)
  (switch-to-buffer (shem-chat-get-buffer user))
  (shem-activity-clean))


(defun shem-activity-mode-line-update ()
  (setq shem-activity-mode-string
        (if shem-activity-list
            (concat "----"
                    (mapconcat
                     (lambda (x)
                       (propertize
                        x
                        'face 'shem-chat-activity-face
                        'local-map (make-mode-line-mouse-map
                                    'mouse-1 `(lambda ()
                                                (interactive)
                                                (shem-activity-switch-to ,x)))
                        'help-echo (concat "Jump to " x  "'s buffer")))
                     shem-activity-list ","))
        ""))
  (force-mode-line-update 'all))


;;;###autoload
(define-minor-mode shem-activity-mode
  :global t
  :init-value t
  (if shem-activity-mode
      (progn
        (add-hook 'window-configuration-change-hook
                  'shem-activity-clean)
        (setq global-mode-string (append global-mode-string
                                         (list '(t shem-activity-mode-string)))))
    (progn
      (remove-hook 'window-configuration-change-hook
                     'shem-activity-clean)
      (setq global-mode-string (delete '(t shem-activity-mode-string)
                                       global-mode-string)))))


(if shem-activity-mode (shem-activity-mode 1))
  

(provide 'shem-chatactivity)