;;; chatwork.el --- ChatWork client for Emacs
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2014, 2015 Masayuki Ataka <masayuki.ataka@gmail.com>

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; URL: https://github.com/ataka/chatwork
;; Keywords: web
;; Version: 0.1

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

;; chatwork.el provides some commands for ChatWork service.

;; Set your ChatWork API token, which you can get from
;; https://www.chatwork.com/service/packages/chatwork/subpackages/api/apply_beta.php
;;
;; Example:
;;
;;  (setq chatwork-token "YOUR CHATWORK API TOKEN")
;;

;;; Code:

(require 'url)
(require 'json)

;;; Custom Variables

(defgroup chatwork nil
  "ChatWork configuration."
  :group 'comm)

(defcustom chatwork-token nil
  "ChatWork API Token."
  :type 'string
  :group 'chatwork)

;; Vars

(defconst chatwork-api-base-url "https://api.chatwork.com/v1"
  "Base URL of ChatWork API.
Refecernce available at http://developer.chatwork.com/ja/endpoints.html")

(defvar chatwork-me-plist nil)
(defvar chatwork-rooms-plist nil)
(defvar chatwork-rooms-alist nil
  "Alist of Rooms which cons cell is `(ROOM_NAME . ROOM_ID)'")
(defvar chatwork-room-history nil)
(defvar chatwork-room-member-alist nil ; FIXME
  "Alist of Room member which cons cell is `(\"alias\" . \"[To:NNNN] Name\")'")
(defvar chatwork-stamp-alist nil
  "Alist of Stamp whic cons cell is `(\"alias\" . \"Stamp strings\")'")
(defvar chatwork-page-delimiter (substring page-delimiter 1 2))

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

(defun chatwork-find-room-id-by-room-name (&optional room-name)
  (let* ((rooms (progn (chatwork-ensure-rooms-alist) chatwork-rooms-alist)))
    (unless room-name
      (setq room-name (let ((completion-ignore-case t)) (completing-read "Room: " rooms nil nil nil 'chatwork-room-history (car chatwork-room-history)))))
    (cdr (assoc room-name rooms))))

;;;###autoload
(defun chatwork-send-message-at-point ()
  "Send message to ChatWork

Call `chatwork-send-message-in-page', if chatwork-mode and mark is not active.
Call `chatwork-send-message-in-region', if mark is active.
Call `chatwork-send-message', if mark is not active and not chatwork-mode."
  (interactive)
  (cond
   ((and (eq major-mode 'chatwork-mode) chatwork-room-name (not mark-active))
    (call-interactively 'chatwork-send-message-in-page))
   (mark-active
    (call-interactively 'chatwork-send-message-in-region))
   (t
    (call-interactively 'chatwork-send-message))))

;;;###autoload
(defun chatwork-send-message (message room-id)
  "Send MESSAGE to ROOM-ID

ROOM-ID is an id number of the room."
  (interactive (list (read-string "Message: ")
		     (chatwork-find-room-id-by-room-name)))
  (chatwork-post-message message room-id))

;;;###autoload
(defun chatwork-send-message-in-region (beg end room-id)
  "Send text in region to ROOM-ID

ROOM-ID is an id number of the room."
  (interactive (let ((room-id (chatwork-find-room-id-by-room-name chatwork-room-name)))
		 (list (region-beginning) (region-end) room-id)))
  (let ((message (buffer-substring-no-properties beg end)))
    (chatwork-post-message message room-id)))

(defun chatwork-send-message-in-page (room-id)
  "Send text in page to ROOM-ID

ROOM-ID is an id number of the room."
  (interactive (let ((room-id (chatwork-find-room-id-by-room-name chatwork-room-name)))
		 (list room-id)))
  (let* ((beg (progn (backward-page) (point)))
	 (end (progn (forward-page) (skip-chars-backward chatwork-page-delimiter) (point)))
	 (message (buffer-substring-no-properties beg end)))
    (chatwork-post-message message room-id))
  (goto-char (point-max))
  (insert "\n" chatwork-page-delimiter))

(defun chatwork-send-stamp (stamp room-id)
  "Send STAMP to ROOM-ID

STAMP is car of cons cell in `chatwork-stamp-alist'.
ROOM-ID is an ad number of the room."
  (interactive (list (completing-read "Stamp: " chatwork-stamp-alist)
                     (chatwork-find-room-id-by-room-name)))
  (chatwork-post-message (cdr (assoc stamp chatwork-stamp-alist))
                         room-id))

(defun chatwork-ensure-rooms-alist ()
  (unless chatwork-rooms-alist
    (chatwork-update-rooms))
  (while (not chatwork-rooms-alist)
    (sleep-for 1)))

(defmacro chatwork-post (path data)
  "Send POST request to ChatWork

PATH should start with \"/\".
DATA should be decoded with `html-hexify-string' if they contains multibyte."
  `(let ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
                                      ("X-ChatWorkToken" . ,chatwork-token)))
         (url-request-data ,data))
     (url-retrieve (chatwork-api-url ,path)
                   'chatwork-post-callback)))

(defun chatwork-post-message (message room-id)
  "Send MESSAGE to ROOM in ChatWork"
  (interactive)
  (chatwork-post (format "/rooms/%d/messages" room-id)
                 (concat "body=" (url-hexify-string message))))

(defun chatwork-post-callback (status)
  (set-buffer (current-buffer))
  (unwind-protect
      (message "done!")
    (kill-buffer)))

(defun chatwork-callback-skip-header ()
  (search-forward "\n\n" nil t))

;;; ChatWork mode

(defvar chatwork-buffer-name-format "*chatwork :%s*")
(defvar chatwork-buffer-name nil)
(make-variable-buffer-local 'chatwork-buffer-name)
(defvar chatwork-room-name nil)
(make-variable-buffer-local 'chatwork-room-name)

;;;###autoload
(defun chatwork ()
  "Call Chatwork major mode"
  (interactive)
  (let* ((room-name (chatwork-select-room))
	 (buffer-name (format chatwork-buffer-name-format room-name)))
    (pop-to-buffer buffer-name)
    (setq chatwork-room-name room-name
	  chatwork-buffer-name buffer-name)
    (chatwork-mode)))

(defun chatwork-mode ()
  (interactive)
  (setq major-mode 'chatwork-mode
	mode-name  "ChatWork")
  (use-local-map chatwork-mode-map)
  (run-hooks 'chatwork-mode-hook))

(defun chatwork-select-room ()
  (let* ((rooms (progn (chatwork-ensure-rooms-alist) chatwork-rooms-alist))
	 (room-name (let ((completion-ignore-case t)) (completing-read "Room: " rooms nil nil nil 'chatwork-room-history (car chatwork-room-history)))))
    room-name))

(defun chatwork-buffer ()
  (concat chatwork-buffer-base-name chatwork-room-name))

;;
;; key map
;;
(defvar chatwork-mode-map nil)
(unless chatwork-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-c" 'chatwork-send-message-at-point)
    ;; Tag
    (define-key map "\C-c\C-i\C-t" 'chatwork-insert-tag-to)
    (define-key map "\C-c\C-i\C-i" 'chatwork-insert-tag-info)
    (define-key map "\C-c\C-i\C-c" 'chatwork-insert-tag-code)
    (define-key map "\C-c\C-i\C-h" 'chatwork-insert-tag-hr)
    (setq chatwork-mode-map map)))


;;; Tag

;; [To:{account_id}] Name
;; [rp aid={account_id} to={room_id}-{message_id}] Name
;; [qt][qtmeta aid={account_id} time={timestamp}] ... [/qt]
;; [info] ... [/info]
;; [info][title]title[/title] ... [/info]
;; [code] ... [/code]
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

;; FIXME
;;
;; (defun chatwork-insert-tag-to (to)
;;   (interactive "sTo: ")
;;   (chatwork-insert-tag "To" (concat ":" to) nil "Name"))
(defun chatwork-insert-tag-to (member)
  (interactive (list (completing-read "To: " chatwork-room-member-alist)))
  (insert (format "%s\n" (cdr (assoc member chatwork-room-member-alist)))))

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

(define-skeleton chatwork-insert-tag-code
  "Insert tag tag."
  > "[code]\n"
  _
  "\n[/code]\n"
)

(defun chatwork-insert-tag-hr ()
  (interactive)
  (chatwork-insert-tag "hr"))

(provide 'chatwork)

;;; chatwork.el ends here
