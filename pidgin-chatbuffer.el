(defgroup pidgin-chat nil "Wrapper for pidgin instant messager"
  :group 'applications)


(defface pidgin-chat-my-message-face
  '((t (:foreground "salmon" :weight bold)))
  "face for own message"
  :group 'pidgin-chat)

(defface pidgin-chat-foriegn-message-face
  '((t (:foreground "SteelBlue1" :weight bold)))
  "face for foriegn message"
  :group 'pidgin-chat)

(defvar pidgin-chat-point-insert nil
  "Position where the message being composed starts")

(defvar pidgin-chat-send-function nil
  "Function for sending a message from a chat buffer.")

(defvar pidgin-chating-with nil)


(defconst pidgin-chat-line-dilimeter "----\n")

(defun pidgin-chat-mode ()
  (kill-all-local-variables)
  ;; Make sure to set this variable somewhere
  (make-local-variable 'pidgin-chat-send-function)

  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5)

  (make-local-variable 'pidgin-chat-point-insert)
  (setq pidgin-chat-point-insert (point-min))

  (setq major-mode 'pidgin-chat-mode
        mode-name "chat")
  (use-local-map pidgin-chat-mode-map)

  (put 'pidgin-chat-mode 'mode-class 'special))

(defvar pidgin-chat-mode-map 
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map nil)
    (define-key map "\r" 'pidgin-chat-buffer-send)
    map))

(defun pidgin-chat-buffer-send ()
  (interactive)
  (let ((body (delete-and-extract-region
               (+ (length pidgin-chat-line-dilimeter) pidgin-chat-point-insert) (point-max))))
    (unless (zerop (length body))
      (funcall pidgin-chat-send-function body))))

(defun pidgin-chat-buffer-display (prompt-function prompt-data output-functions output-data tail)
 ;  (if (not pidgin-chat-point-insert)
 ;     (setq pidgin-chat-point-insert (point-max)))
  (let ((at-insert-point (eq (point) pidgin-chat-point-insert))
        outputp)
    (save-excursion
      (goto-char pidgin-chat-point-insert)
      (setq outputp
            (pidgin-chat-buffer-display-at-point prompt-function prompt-data output-functions output-data tail))
      (setq pidgin-chat-point-insert (point))
      (set-text-properties pidgin-chat-point-insert (point-max) nil))

    (when at-insert-point
      (goto-char pidgin-chat-point-insert))
    outputp))

(defun pidgin-chat-buffer-display-at-point (prompt-function prompt-data output-functions output-data tail)
  (let ((inhibit-read-only t)
        (beg (point))
        (point-insert (set-marker (make-marker) pidgin-chat-point-insert)))
    (set-marker-insertion-type point-insert t)

    (dolist (printer output-functions)
      (funcall printer output-data)
      (unless (bolp)
        (insert "\n")))

    (unless (eq (point) beg)
      (let ((end (point-marker)))
        (unless tail
          (goto-char beg)
          (funcall prompt-function prompt-data)
          (goto-char end))
        (put-text-property beg end 'read-only t)
        (put-text-property beg end 'front-sticky t)
        (put-text-property beg end 'rear-nonsticky t)

        ;;add message to history
        (write-region beg end (concat pidgin-messenger-directory pidgin-chating-with ".txt") t 'no-echo)

        ;; this is always non-nil, so we return that
        (setq pidgin-chat-point-insert (marker-position point-insert))))))


(defun pidgin-chat-send (body)
  (pidgin-send-message pidgin-chating-with body)
  (pidgin-chat-buffer-display 'pidgin-chat-self-prompt
                            nil
                            '(insert)
                            (propertize
                             body
                             'face 'default)
                            nil))

(defun pidgin-chat-recieve(name from body &optional tail)
  ;;(with-current-buffer (get-buffer-create "*chat-debug*")
  ;;  (insert body))
      
  (let* ((buf-name (pidgin-chat-get-buffer from))
         (curr-buf (or (get-buffer buf-name) (pidgin-chat-create-buffer from))))
    (with-current-buffer curr-buf
      (pidgin-chat-buffer-display 'pidgin-chat-foriegn-prompt
                                name
                                '(insert)
                                (propertize
                                 body
                                 'face 'default)
                                tail)))
  (pidgin-activity-add from))

(defun pidgin-chat-self-prompt (timestamp)
  (insert (propertize
           (concat "["(format-time-string "%H:%M") "] " (system-name) "> ")
           'face 'pidgin-chat-my-message-face)))

(defun pidgin-chat-foriegn-prompt (name)
  (insert (propertize
           (concat "["(format-time-string "%H:%M") "] " name "> ")
           'face 'pidgin-chat-foriegn-message-face)))

(defun pidgin-chat-get-buffer (chat-with)
  (concat "*chat:" chat-with "*"))

(defun pidgin-chat-create-buffer (chat-with)
  (with-current-buffer (get-buffer-create (pidgin-chat-get-buffer chat-with))
    (insert pidgin-chat-line-dilimeter)
    (if (not (eq major-mode 'pidgin-chat-mode)) (pidgin-chat-mode))
    (make-local-variable 'pidgin-chating-with)
    (setq pidgin-chating-with chat-with)
    (setq pidgin-chat-send-function 'pidgin-chat-send)
    (make-local-variable 'pidgin-chat-earliest-backlog)
    (set-input-method pidgin-default-input-method)
    (current-buffer)))



(defun pidgin-protocol-user-name (user &optional protocol)
  (concat (if user user "unknown") (if protocol (concat pidgin-protocol-delimeter protocol ) "")))

(defun pidgin-user-list (&optional protocol)
  (pidgin-buddy-list protocol))

(defun pidgin-chat-with (&optional protocol-id)
  (interactive (list current-prefix-arg))
  (let* ((user (let ((protocol (if (and current-prefix-arg
                                        (numberp current-prefix-arg)
                                        (eq current-prefix-arg 1))
                                   pidgin-icq-protocol
                                 pidgin-jabber-protocol)))
                 (pidgin-protocol-user-name
                  (funcall pidgin-completing-read "chat with: " (pidgin-user-list protocol))
                  protocol)))
         (curr-buf (or (get-buffer (pidgin-chat-get-buffer user)) (pidgin-chat-create-buffer user))))
    (switch-to-buffer curr-buf)))

(provide 'pidgin-chatbuffer)



