(defgroup shem-chat nil "Herocraft instant messaging"
  :group 'applications)


(defface shem-chat-my-message-face
  '((t (:foreground "salmon" :weight bold)))
  "face for own message"
  :group 'shem-chat)

(defface shem-chat-foriegn-message-face
  '((t (:foreground "SteelBlue1" :weight bold)))
  "face for foriegn message"
  :group 'shem-chat)

(defvar shem-chat-point-insert nil
  "Position where the message being composed starts")

(defvar shem-chat-send-function nil
  "Function for sending a message from a chat buffer.")


(defconst shem-chat-line-dilimeter "----\n")

(defun shem-chat-mode ()
  (kill-all-local-variables)
  ;; Make sure to set this variable somewhere
  (make-local-variable 'shem-chat-send-function)

  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 5)

  (make-local-variable 'shem-chat-point-insert)
  (setq shem-chat-point-insert (point-min))

  (setq major-mode 'shem-chat-mode
        mode-name "chat")
  (use-local-map shem-chat-mode-map)

  (put 'shem-chat-mode 'mode-class 'special))

(defvar shem-chat-mode-map 
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map nil)
    (define-key map "\r" 'shem-chat-buffer-send)
    map))

(defun shem-chat-buffer-send ()
  (interactive)
  (let ((body (delete-and-extract-region
               (+ (length shem-chat-line-dilimeter) shem-chat-point-insert) (point-max))))
    (unless (zerop (length body))
      (funcall shem-chat-send-function body))))

(defun shem-chat-buffer-display (prompt-function prompt-data output-functions output-data tail)
 ;  (if (not shem-chat-point-insert)
 ;     (setq shem-chat-point-insert (point-max)))
  (let ((at-insert-point (eq (point) shem-chat-point-insert))
        outputp)
    (save-excursion
      (goto-char shem-chat-point-insert)
      (setq outputp
            (shem-chat-buffer-display-at-point prompt-function prompt-data output-functions output-data tail))
      (setq shem-chat-point-insert (point))
      (set-text-properties shem-chat-point-insert (point-max) nil))

    (when at-insert-point
      (goto-char shem-chat-point-insert))
    outputp))

(defun shem-chat-buffer-display-at-point (prompt-function prompt-data output-functions output-data tail)
  (let ((inhibit-read-only t)
        (beg (point))
        (point-insert (set-marker (make-marker) shem-chat-point-insert)))
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
        (write-region beg end (concat "~/.messager/" shem-chatting-with ".txt") t 'no-echo)

        ;; this is always non-nil, so we return that
        (setq shem-chat-point-insert (marker-position point-insert))))))


(defun shem-chat-send (body)
  (shem-chat-send-message shem-chatting-with body)
  (shem-chat-buffer-display 'shem-chat-self-prompt
                            nil
                            '(insert)
                            (propertize
                             body
                             'face 'default)
                            nil))

(defun shem-chat-recieve(name from body &optional tail)
  ;;(with-current-buffer (get-buffer-create "*chat-debug*")
  ;;  (insert body))
      
  (let* ((buf-name (shem-chat-get-buffer from))
         (curr-buf (or (get-buffer buf-name) (shem-chat-create-buffer from))))
    (with-current-buffer curr-buf
      (shem-chat-buffer-display 'shem-chat-foriegn-prompt
                                name
                                '(insert)
                                (propertize
                                 body
                                 'face 'default)
                                tail)))
  (shem-activity-add from))

(defun shem-chat-self-prompt (timestamp)
  (insert (propertize
           (concat "["(format-time-string "%H:%M") "] " (system-name) "> ")
           'face 'shem-chat-my-message-face)))

(defun shem-chat-foriegn-prompt (name)
  (insert (propertize
           (concat "["(format-time-string "%H:%M") "] " name "> ")
           'face 'shem-chat-foriegn-message-face)))

(defun shem-chat-get-buffer (chat-with)
  (concat "*chat:" chat-with "*"))

(defun shem-chat-create-buffer (chat-with)
  (with-current-buffer (get-buffer-create (shem-chat-get-buffer chat-with))
    (insert shem-chat-line-dilimeter)
    (if (not (eq major-mode 'shem-chat-mode)) (shem-chat-mode))
    (make-local-variable 'shem-chatting-with)
    (setq shem-chatting-with chat-with)
    (setq shem-chat-send-function 'shem-chat-send)
    (make-local-variable 'shem-chat-earliest-backlog)
    (set-input-method shem-default-input-method)
    (current-buffer)))



(defun shem-protocol-user-name (user &optional protocol)
  (concat (if user user "unknown") (if protocol (concat shem-protocol-delimeter protocol ) "")))

(defun shem-user-list (&optional protocol)
  (shem-pidgin-buddy-list protocol))

(defun shem-chat-with (&optional protocol-id)
  (interactive (list current-prefix-arg))
  (let* ((user (let ((protocol (if (and current-prefix-arg
                                        (numberp current-prefix-arg)
                                        (eq current-prefix-arg 1))
                                   shem-icq-protocol
                                 shem-jabber-protocol)))
                 (shem-protocol-user-name
                  (ido-completing-read "chat with: " (shem-pidgin-user-list protocol))
                  protocol)))
         (curr-buf (or (get-buffer (shem-chat-get-buffer user)) (shem-chat-create-buffer user))))
    (switch-to-buffer curr-buf)))

(provide 'shem-chatbuffer)



