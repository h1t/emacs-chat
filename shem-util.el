(defun shem-replace-regexp (regexp to-string)
  (while (re-search-forward regexp nil t)
    (replace-match to-string nil nil)))

(defun shem-string-befor-p (prefix str)
  (string-match (concat "^" prefix ".*") str))

(defun shem-string-after-p (postfix str)
  (string-match (concat postfix "$") str))

(provide 'shem-util)
