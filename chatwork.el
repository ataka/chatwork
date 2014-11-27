;;; chatwork.el --- ChatWork client for Emacs
;; -*- Mode: Emacs-Lisp -*-

;;; Code:

(provide 'chatwork)

;;; Tag

;; [To:{account_id}] Name
;; [rp aid={account_id} to={room_id}-{message_id}] Name
;; [qt][qtmeta aid={account_id} time={timestamp}] ... [/qt]
;; [info] ... [/info]
;; [info][title]title[/title] ... [/info]
;; [hr]
;; [picon:{account_id}]
;; [piconname:{account_id}]

(defun chatwork-insert-tag (tag &optional attr close following)
  (let* ((open-tag (if attr (format "[%s%s]" tag attr) (format "[%s]" tag)))
	 (close-tag (format "[/%s]" tag)))
    (insert open-tag)
    (when close
      (save-excursion
	(insert close-tag)))
    (when following
      (insert following " "))))

(defun chatwork-concat-heading-space (str)
  (concat (when str " ") str))

(defun chatwork-insert-tag-to (to)
  (interactive "sTo: ")
  (chatwork-insert-tag "To" (concat ":" to) nil "Name"))

(defun chatwork-insert-tag-info ()
  (interactive)
  (chatwork-insert-tag "info" nil t))

(defun chatwork-insert-tag-info-with-title (title)
  (interactive "sTitle: ")
  (chatwork-insert-tag-info)
  (progn
    (chatwork-insert-tag "title" nil t)
    (insert title))
  (search-forward "[/title]" nil t))

(defun chatwork-insert-tag-hr ()
  (interactive)
  (chatwork-insert-tag "hr"))

;;; chatwork.el ends here
