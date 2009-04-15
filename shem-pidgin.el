(require 'dbus)
(require 'xml)

;;http://habahaba.jrudevels.org/

(defconst shem-icq-protocol "icq")

(defconst shem-jabber-protocol "xmpp")

(defvar shem-pidgin-accounts nil)

(defvar shem-pidgin-all-user-list nil)

(defvar shem-pidgin-regexp-filter
  '(("<br>\\|<br/>" "\n")
    ("<a href='.*'>\\(.*\\)</a>" "\\1")))

(defun shem-pidgin-recieve-signal (account sender text conversation flags)
  (let* ((protocol (car (rassoc account shem-pidgin-accounts)))
         (message (shem-pidgin-parse-message text))
         (sender-name (car (rassoc (list (car (split-string sender "/")))
                                   (shem-pidgin-user-list protocol)))))
    (shem-chat-recieve
     (shem-protocol-user-name sender-name)
     (shem-protocol-user-name sender-name protocol)
     message)))


(defun shem-pidgin-parse-message (message)
  (message (concat "from jabber: '" message "'"))
  (with-temp-buffer
    (insert message)
    (mapc (lambda (regexp-info)
            (goto-char (point-min))
            (apply 'shem-replace-regexp regexp-info))
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
  (setq shem-pidgin-accounts (shem-pidgin-account-list))
  (setq shem-pidgin-all-user-list
        (mapcar (lambda (account-info)
                  (list (car account-info)
                        (shem-pidgin-buddy-list (cdr account-info))))
                shem-pidgin-accounts)))



(defun shem-pidgin-send-message (to message)
  (let* ((sender (split-string to shem-protocol-delimeter))
         (name (car sender))
         (protocol (second sender)))
    (shem-dbus-pidgin-send-message
     (cdr (assoc protocol shem-pidgin-accounts))
     (second (assoc name (shem-pidgin-user-list protocol)))
     message)))

(defmacro shem-dbus-purple-call-method (method &rest args)
  `(dbus-call-method :session "im.pidgin.purple.PurpleService"
                         "/im/pidgin/purple/PurpleObject"
                         "im.pidgin.purple.PurpleInterface"
                         ,method ,@args))

(defun shem-pidgin-account-list ()
  (mapcar (lambda (account)
            (cons (downcase
                   (shem-dbus-purple-call-method
                    "PurpleAccountGetProtocolName"
                    :int32 account))
                  account))
          (shem-dbus-purple-call-method "PurpleAccountsGetAllActive")))



(defun shem-dbus-pidgin-send-message (account recipient message)
  (let* ((conversation (shem-dbus-purple-call-method
                        "PurpleConversationNew"
                        1 :int32 account recipient))
         (im (shem-dbus-purple-call-method
              "PurpleConvIm"
              :int32 conversation)))
      (shem-dbus-purple-call-method
       "PurpleConvImSend"
       :int32 im (string-as-unibyte message))))


(defun shem-pidgin-user-list (protocol)
  (second (assoc protocol shem-pidgin-all-user-list)))

(defun shem-pidgin-buddy-list (account)
  (mapcar (lambda (buddy)
            (list (shem-dbus-purple-call-method "PurpleBuddyGetAlias" :int32 buddy)
                  (shem-dbus-purple-call-method "PurpleBuddyGetName"  :int32 buddy)))
          (shem-dbus-purple-call-method "PurpleFindBuddies" :int32 account "")))

(provide 'shem-pidgin)

