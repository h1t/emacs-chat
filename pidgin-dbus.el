(require 'dbus)
(require 'xml)

;;http://habahaba.jrudevels.org/

(defconst pidgin-icq-protocol "icq")

(defconst pidgin-jabber-protocol "xmpp")

(defvar pidgin-accounts nil)

(defvar pidgin-all-user-list nil)

(defvar pidgin-regexp-filter
  '(("<br>\\|<br/>" "\n")
    ("<a href='.*'>\\(.*\\)</a>" "\\1")))

(defun pidgin-recieve-signal (account sender text conversation flags)
  (let* ((protocol (car (rassoc account pidgin-accounts)))
         (message (pidgin-parse-message text))
         (sender-name (car (rassoc (list (car (split-string sender "/")))
                                   (pidgin-user-list protocol)))))
    (pidgin-chat-recieve
     (pidgin-protocol-user-name sender-name)
     (pidgin-protocol-user-name sender-name protocol)
     message)))


(defun pidgin-parse-message (message)
  (message "%s" (format "from jabber: '%s'" message))
  (with-temp-buffer
    (insert message)
    (mapc (lambda (regexp-info)
            (goto-char (point-min))
            (apply 'pidgin-replace-regexp regexp-info))
          pidgin-regexp-filter)
    (sgml-mode)
    (sgml-tags-invisible 0)
    (buffer-string)))

(defun pidgin-init ()
  (ignore-errors
    (dbus-register-signal :session "im.pidgin.purple.PurpleService"
                          "/im/pidgin/purple/PurpleObject"
                          "im.pidgin.purple.PurpleInterface"
                          "ReceivedImMsg"
                          'pidgin-recieve-signal))
  (setq pidgin-accounts (pidgin-account-list))
  (setq pidgin-all-user-list
        (mapcar (lambda (account-info)
                  (list (car account-info)
                        (pidgin-buddy-list (cdr account-info))))
                pidgin-accounts)))



(defun pidgin-send-message (to message)
  (let* ((sender (split-string to pidgin-protocol-delimeter))
         (name (car sender))
         (protocol (second sender)))
    (pidgin-dbus-send-message
     (cdr (assoc protocol pidgin-accounts))
     (second (assoc name (pidgin-user-list protocol)))
     message)))

(defmacro pidgin-dbus-purple-call-method (method &rest args)
  `(dbus-call-method :session "im.pidgin.purple.PurpleService"
                     "/im/pidgin/purple/PurpleObject"
                     "im.pidgin.purple.PurpleInterface"
                     ,method ,@args))

(defun pidgin-account-list ()
  (mapcar (lambda (account)
            (cons (downcase
                   (pidgin-dbus-purple-call-method
                    "PurpleAccountGetProtocolName"
                    :int32 account))
                  account))
          (pidgin-dbus-purple-call-method "PurpleAccountsGetAllActive")))



(defun pidgin-dbus-send-message (account recipient message)
  (let* ((conversation (pidgin-dbus-purple-call-method
                        "PurpleConversationNew"
                        :int32 1 :int32 account recipient))
         (im (pidgin-dbus-purple-call-method
              "PurpleConvIm"
              :int32 conversation)))
    (pidgin-dbus-purple-call-method
     "PurpleConvImSend"
     :int32 im (string-as-unibyte message))))


(defun pidgin-user-list (protocol)
  (second (assoc protocol pidgin-all-user-list)))

(defun pidgin-buddy-list (account)
  (mapcar (lambda (buddy)
            (list (pidgin-dbus-purple-call-method "PurpleBuddyGetAlias" :int32 buddy)
                  (pidgin-dbus-purple-call-method "PurpleBuddyGetName"  :int32 buddy)))
          (pidgin-dbus-purple-call-method "PurpleFindBuddies" :int32 account "")))

(provide 'pidgin-dbus)

