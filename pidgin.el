(require 'cl)
(require 'pidgin-chatbuffer)
(require 'pidgin-dbus)
(require 'pidgin-chatactivity)

(defconst pidgin-protocol-delimeter "-")

(defvar pidgin-default-input-method default-input-method)

(defvar pidgin-completing-read 'completing-read)

(defvar pidgin-messenger-directory "~/.messenger")

(defun pidgin-connect ()
  (when (and (fboundp 'ido-completing-read)
             ido-mode)
    (setq pidgin-completing-read 'ido-completing-read)))
  (pidgin-init)

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
