(require 'shem-chatbuffer)
(require 'shem-pidgin)
(require 'shem-chatactivity)

(defconst shem-protocol-delimeter "-")

(defvar shem-default-input-method "russian-computer")

(defun shem-chat-connect ()
  (shem-pidgin-init))

(defvar shem-chat-from nil)
(defvar shem-chat-to nil)
(defvar shem-chat-message nil)

(defun shem-chat-send-message (to message)
  (shem-pidgin-send-message to message))

(defun shem-replace-regexp (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun shem-string-befor-p (prefix str)
  (string-match (concat "^" prefix ".*") str))

(defun shem-string-after-p (postfix str)
  (string-match (concat postfix "$") str))

(defun shem-delete-if (fn list)
  (let (res)
    (mapc (lambda (elem)
            (unless (funcall fn elem)
              (setq res (cons elem res))))
          list)
    (nreverse res)))
  
(provide 'shem-chatcore)
