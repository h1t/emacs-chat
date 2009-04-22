(require 'cl)
(require 'pidgin-chatbuffer)
(require 'pidgin-dbus)
(require 'pidgin-chatactivity)

(defconst pidgin-protocol-delimeter "-")

(defvar pidgin-default-input-method "russian-computer")

(defvar pidgin-completing-read 'completing-read)

(defun pidgin-connect ()
  (when (and (fboundp 'ido-completing-read)
             ido-mode)
    (setq pidgin-completing-read 'ido-completing-read)))
  (pidgin-init)

(defvar pidgin-chat-from nil)
(defvar pidgin-chat-to nil)
(defvar pidgin-chat-message nil)

(defun pidgin-chat-send-message (to message)
  (pidgin-send-message to message))

(defun pidgin-replace-regexp (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun pidgin-string-befor-p (prefix str)
  (string-match (concat "^" prefix ".*") str))

(defun pidgin-string-after-p (postfix str)
  (string-match (concat postfix "$") str))

(defun pidgin-delete-if (fn list)
  (let (res)
    (mapc (lambda (elem)
            (unless (funcall fn elem)
              (setq res (cons elem res))))
          list)
    (nreverse res)))
  
(provide 'pidgin)
