;;; chatwork.el --- ChatWork client for Emacs
;; -*- Mode: Emacs-Lisp -*-

;;; Code:

(require 'url)
(require 'json)

;;; Custom Variables

;; FIXME: Use defcustom

(defvar chatwork-token nil)

;; Vars

(defconst chatwork-api-base-url "https://api.chatwork.com/v1"
  "Base URL of ChatWork API.
Refecernce available at http://developer.chatwork.com/ja/endpoints.html")

(defvar chatwork-me-plist nil)

;;; Connectivity

(defun chatwork-api-url (endpoint)
  "Return URL for ChatWork API with `chatwork-api-base-url' and ENDPOINT"
  (concat chatwork-api-base-url endpoint))

(defun chatwork-me ()
  (let ((url-request-extra-headers `(("X-ChatWorkToken" . ,chatwork-token))))
    (url-retrieve (chatwork-api-url "/me") 'chatwork-me-callback nil t)))

(defun chatwork-me-callback (status)
  (unless (plist-get status :error)
    (let ((json-object-type 'plist))
      (set-buffer (current-buffer))
      (unwind-protect
	  (let ((json-data (progn (chatwork-callback-skip-header)
				  (json-read))))
	    (setq chatwork-me-plist json-data))
	(kill-buffer)))))

(defun chatwork-callback-skip-header ()
  (search-forward "\n\n" nil t))

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

(provide 'chatwork)

;;; chatwork.el ends here
