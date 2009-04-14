(require 'shem-chatbuffer)
(require 'shem-pidgin)
(require 'shem-chatactivity)

(defun shem-chat-connect ()
  (shem-pidgin-init))

(defvar shem-chat-from nil)
(defvar shem-chat-to nil)
(defvar shem-chat-message nil)

(defun shem-chat-send-message (to message)
  (shem-pidgin-send-message to message))
  
(provide 'shem-chatcore)
