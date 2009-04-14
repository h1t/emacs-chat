(require 'dbus)
(require 'xml)

;;http://habahaba.jrudevels.org/

(defconst shem-icq-protocol "icq")

(defconst shem-jabber-protocol "xmpp")

(defvar shem-pidgin-accounts nil)

(defvar shem-icq-name-list nil)

(defvar shem-jabber-name-list nil)

(defvar shem-pidgin-regexp-filter
  '(("<br>\\|<br/>" "\n")
    ("<a href='.*'>\\(.*\\)</a>" "\\1")))

(defun shem-pidgin-recieve-signal (account sender text conversation flags)
  (let* ((protocol (cdr (assoc account  shem-pidgin-accounts)))
        (message (if (string-equal protocol shem-jabber-protocol)
                     (shem-pidgin-parse-jabber-message text)
                   text)))
    (shem-chat-recieve
     (shem-pidgin-sender-name protocol sender t)
     (shem-pidgin-sender-name protocol sender) message)))


(defun shem-pidgin-sender-name (protocol sender-id &optional only-name)
  (shem-protocol-user-name
   (car (find sender-id
              (shem-pidgin-buddy-list protocol)
              :key #'second
              :test (if (string-equal protocol shem-jabber-protocol)
                        #'shem-pidgin-jabber-user-compare
                      #'string-equal)))
   (unless only-name protocol)))

(defun shem-pidgin-jabber-user-compare (user1 user2)
  (string-equal (car (split-string user1 "/"))
                (car (split-string user2 "/"))))


(defun shem-pidgin-parse-jabber-message (message)
  (message (concat "from jabber: '" message "'"))
  (with-temp-buffer
    (insert message)
    (mapc (lambda (regexp-info)
            (goto-char (point-min))
            (shem-replace-regexp (car regexp-info) (cadr regexp-info)))
          shem-pidgin-regexp-filter)
    (let* ((body (xml-parse-region (point-min) (point-max)))
           (xml  (cddr (if (assoc 'body body)
                           (assoc 'body body)
                         (assoc 'body (car body)))))
          (res ""))
      (labels ((shem-visitor (span-list)
                             (cond ((stringp span-list) (setq res (concat res span-list)))
                                   ((eq (car span-list) 'span) (shem-visitor (cddr span-list)))
                                   (t (dolist (elem span-list)
                                        (shem-visitor elem))))))
        (shem-visitor xml))
      res)))

(defun shem-pidgin-init ()
  (ignore-errors
    (dbus-register-signal :session "im.pidgin.purple.PurpleService"
                          "/im/pidgin/purple/PurpleObject"
                          "im.pidgin.purple.PurpleInterface"
                          "ReceivedImMsg"
                          'shem-pidgin-recieve-signal))
  (setq shem-pidgin-accounts (get-account-list)))


(defun shem-pidgin-send-message (to message)
  (let ((sender (split-string to shem-protocol-delimeter)))
    (shem-dbus-pidgin-send-message
     (car (find (second sender) shem-pidgin-accounts :test #'string-equal :key #'cdr))
     (second (assoc (car sender) (shem-pidgin-buddy-list (second sender))))
     message)))

(defmacro shem-dbus-purple-call-method (method &rest args)
  `(dbus-call-method :session "im.pidgin.purple.PurpleService"
                         "/im/pidgin/purple/PurpleObject"
                         "im.pidgin.purple.PurpleInterface"
                         ,method ,@args))

(defun get-account-list ()
  (mapcar (lambda (account)
            (cons account (downcase
                           (shem-dbus-purple-call-method
                            "PurpleAccountGetProtocolName"
                            :int32 account))))
          (shem-dbus-purple-call-method "PurpleAccountsGetAllActive")))



(defun shem-dbus-pidgin-send-message (account recipient message)
  ;; (message (number-to-string account))
  ;; (message (number-to-string recipient))
  ;;(message message)
  (let* ((conversation (shem-dbus-purple-call-method
                        "PurpleConversationNew"
                        1 :int32 account recipient))
         (im (shem-dbus-purple-call-method
              "PurpleConvIm"
              :int32 conversation)))
      (shem-dbus-purple-call-method
       "PurpleConvImSend"
       :int32 im (string-as-unibyte message))))


(defun shem-pidgin-buddy-list (protocol)
  (if (string-equal protocol shem-icq-protocol)
      shem-icq-name-list
    shem-jabber-name-list))

(provide 'shem-pidgin)