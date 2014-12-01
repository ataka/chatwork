;;; chatwork.el --- ChatWork client for Emacs
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2014 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; URL: https://github.com/ataka/chatwork
;; Keywords: web

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

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
(defvar chatwork-rooms-plist nil)
(defvar chatwork-rooms-alist nil
  "Alist of Rooms which cons cell is `(ROOM_ID . ROOM_NAME)'")

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

(defalias 'chatwork-update-rooms 'chatwork-get-rooms)

(defun chatwork-get-rooms ()
  (interactive)
  (let ((url-request-extra-headers `(("X-ChatWorkToken" . ,chatwork-token))))
    (url-retrieve (chatwork-api-url "/rooms") 'chatwork-get-rooms-callback)))

(defun chatwork-get-rooms-callback (status)
  (unless (plist-get status :error)
    (let ((json-object-type 'plist))
      (set-buffer (current-buffer))
      (unwind-protect
	  (let ((json-data (progn (chatwork-callback-skip-header)
				  (json-read))))
	    (setq chatwork-rooms-plist json-data)
	    (setq chatwork-rooms-alist
		  (mapcar (lambda (room)
			    (let ((room-id   (plist-get room :room_id))
				  (room-name (plist-get room :name)))
			      (cons room-name room-id)))
			  chatwork-rooms-plist)))
	(kill-buffer)))))

(defun chatwork-find-room-id-by-room-name ()
    (let* ((rooms (progn (chatwork-ensure-rooms-alist) chatwork-rooms-alist))
	   (room-name (completing-read "Room: " rooms)))
      (cdr (assoc room-name rooms))))

(defun chatwork-send-message (message room-id)
  (interactive (list (read-string "Message: ")
		     (chatwork-find-room-id-by-room-name)))
  (chatwork-post-message message room-id))

(defun chatwork-send-message-in-region (message room-id)
  (interactive (let ((room-id (chatwork-find-room-id-by-room-name)))
		 (list (buffer-substring-no-properties
			(region-beginning) (region-end))
		       room-id)))
  (chatwork-post-message message room-id))

(defun chatwork-ensure-rooms-alist ()
  (unless chatwork-rooms-alist
    (chatwork-update-rooms))
  (while (not chatwork-rooms-alist)
    (sleep-for 1)))

(defun chatwork-post-message (message room-id)
  "Send MESSAGE to ROOM in ChatWork"
  (interactive)
  (let ((url-request-method "POST")
	(url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
				     ("X-ChatWorkToken" . ,chatwork-token)))
	(url-request-data (concat "body=" (url-hexify-string message))))
    (url-retrieve (chatwork-api-url (format "/rooms/%d/messages" room-id))
		  'chatwork-post-callback)))

(defun chatwork-post-callback (status)
  (set-buffer (current-buffer))
  (unwind-protect
      (message "done!")
    (kill-buffer)))

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
