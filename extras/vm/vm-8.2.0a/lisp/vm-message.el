;;; vm-message.el --- Macros and functions dealing with accessing VM
;; message struct fields 
;;
;; This file is part of VM
;;
;; Copyright (C) 1989-1997 Kyle E. Jones
;; Copyright (C) 2003-2006 Robert Widhopf-Fenk
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Code:

(provide 'vm-message)

(declare-function vm-mime-encode-words-in-string "vm-mime" (string))
(declare-function vm-reencode-mime-encoded-words-in-string 
		  "vm-mime" (string))
(declare-function vm-reencode-mime-encoded-words-in-tokenized-summary
		  "vm-mime" (summary))
(declare-function vm-mark-for-summary-update 
		  "vm-folder" (m &optional dont-kill-cache))
(declare-function vm-stuff-virtual-message-data 
		  "vm-folder" (message))
(declare-function vm-reorder-message-headers 
		  "vm-folder" (message &optional keep-list discard-regexp))
(declare-function vm-mark-folder-modified-p
		  "vm-folder" (buffer))
(declare-function vm-clear-modification-flag-undos 
		  "vm-undo" ())
(declare-function vm-build-threads 
		  "vm-undo" (message-list))
(declare-function vm-unthread-message
		  "vm-thread" (message &key message-changing))
(declare-function vm-present-current-message 
		  "vm-page" ())
(declare-function vm-zip-vectors "vm-misc" (v1 v2))
(declare-function vm-zip-lists "vm-misc.el" (list1 list2) t)


;; current message
(defsubst vm-current-message ()
  "Returns the currently selected message in the VM folder.  It
works in all VM buffers."
  (with-current-buffer (or vm-mail-buffer (current-buffer))
    (car vm-message-pointer)))

;; message struct
(defconst vm-location-data-vector-length 6)
(defconst vm-message-fields 
  [:location-data :softdata :attributes :cached-data :mirror-data])
(defsubst vm-location-data-of (message) (aref message 0))
(defsubst vm-softdata-of (message) (aref message 1))
(defsubst vm-attributes-of (message) (aref message 2))
(defsubst vm-cached-data-of (message) (aref message 3))
(defsubst vm-mirror-data-of (message) (aref message 4))
(defsubst vm-set-location-data-of (message vdata) (aset message 0 vdata))
(defsubst vm-set-softdata-of (message data) (aset message 1 data))
(defsubst vm-set-attributes-of (message attrs) (aset message 2 attrs))
(defsubst vm-set-cached-data-of (message cache) (aset message 3 cache))
(defsubst vm-set-mirror-data-of (message data) (aset message 4 data))

;; data that is always shared with virtual folders
(defconst vm-location-data-fields
  [:start :headers :vheaders :text :text-end :end])
;; where message begins starting at the message separator in the folder
(defsubst vm-start-of (message)
  (aref (aref message 0) 0))
;; where headers start (From_ line)
(defsubst vm-headers-of (message)
  (aref (aref message 0) 1))
;; where visible headers start
(defun vm-vheaders-of (message)
  (or (aref (aref message 0) 2)
      (progn (vm-reorder-message-headers message)
	     (aref (aref message 0) 2))))
;; where text section starts
(defsubst vm-text-of (message)
  (or (aref (aref message 0) 3) 
      (progn (vm-find-and-set-text-of message)
	     (aref (aref message 0) 3))))
;; where text portion of message ends
(defsubst vm-text-end-of (message)
  (aref (aref message 0) 4))
;; where message ends
(defsubst vm-end-of (message)
  (aref (aref message 0) 5))

;; soft data vector
(defconst vm-softdata-vector-length 23)
(defconst vm-softdata-fields
  [:number :padded-number :mark :su-start :su-end :real-message-sym
	   :reverse-link-sym :message-type :message-id-number :buffer
	   :thread-indentation :thread-list
	   :babyl-frob-flag :saved-virtual-attributes
	   :saved-virtual-mirror-data :virtual-summary 
	   :mime-layout :mime-encoded-header-flag
	   :su-summary-mouse-track-overlay :message-access-method
	   :thread-subtree :mirrored-message-sym :thread-indentation-offset])
(defsubst vm-number-of (message)
  (aref (aref message 1) 0))
(defsubst vm-padded-number-of (message)
  (aref (aref message 1) 1))
(defsubst vm-mark-of (message)
  (aref (aref message 1) 2))
;; start of summary line
(defsubst vm-su-start-of (message)
  (aref (aref message 1) 3))
;; end of summary line
(defsubst vm-su-end-of (message)
  (aref (aref message 1) 4))
;; symbol whose value is the real message.
(defsubst vm-real-message-sym-of (message)
  (aref (aref message 1) 5))
;; real message
(defsubst vm-real-message-of (message)
  (symbol-value (aref (aref message 1) 5)))
;; link to previous message in the message list
(defsubst vm-reverse-link-of (message)
  (symbol-value (aref (aref message 1) 6)))
;; message type
(defsubst vm-message-type-of (message)
  (aref (aref message 1) 7))
;; number that uniquely identifies each message
;; this is for the set handling stuff
(defsubst vm-message-id-number-of (message)
  (aref (aref message 1) 8))
;; folder buffer of this message
(defsubst vm-buffer-of (message)
  (aref (aref message 1) 9))
;; cache thread indentation value
(defsubst vm-thread-indentation-of (message)
  (aref (aref message 1) 10))
;; list of symbols from vm-thread-obarray that give this message's lineage
(defsubst vm-thread-list-of (message)
  (aref (aref message 1) 11))
;; babyl header frob flag (0 or 1 at beginning of message)
(defsubst vm-babyl-frob-flag-of (message)
  (aref (aref message 1) 12))
;; saved attributes, if message was switched from unmirrored to mirrored
(defsubst vm-saved-virtual-attributes-of (message)
  (aref (aref message 1) 13))
;; saved mirror data, if message was switched from unmirrored to mirrored
(defsubst vm-saved-virtual-mirror-data-of (message)
  (aref (aref message 1) 14))
;; summary for unmirrored virtual message
(defsubst vm-virtual-summary-of (message)
  (aref (aref message 1) 15))
;; MIME layout information; types, ids, positions, etc. of all MIME entities
(defsubst vm-mime-layout-of (message)
  (aref (aref message 1) 16))
(defsubst vm-mime-encoded-header-flag-of (message)
  (aref (aref message 1) 17))
(defsubst vm-su-summary-mouse-track-overlay-of (message)
  (aref (aref message 1) 18))
(defsubst vm-message-access-method-of (message)
  (aref (aref message 1) 19))
(defsubst vm-thread-subtree-of (message)
  (aref (aref message 1) 20))
(defsubst vm-mirrored-message-sym-of (message)
  (aref (aref message 1) 21))
(defsubst vm-mirrored-message-of (message)
  (symbol-value (aref (aref message 1) 21)))
(defsubst vm-thread-indentation-offset-of (message)
  (aref (aref message 1) 22))

;; message attribute vector
(defconst vm-attributes-vector-length 16)
(defconst vm-attributes-fields
  [:new-flag :unread-flag :deleted-flag :filed-flag :replied-flag
	     :written-flag :forwarded-flag :edited-flag
	     :redistributed-flag
	     :flagged-flag :folded-flag :watched-flag :ignored-flag
	     :read-receipt-flag :read-receipt-sent-flag :attachments-flag]) 
(defsubst vm-new-flag (message) (aref (aref message 2) 0))
(defsubst vm-unread-flag (message) (aref (aref message 2) 1))
(defsubst vm-deleted-flag (message) (aref (aref message 2) 2))
(defsubst vm-filed-flag (message) (aref (aref message 2) 3))
(defsubst vm-replied-flag (message) (aref (aref message 2) 4))
(defsubst vm-written-flag (message) (aref (aref message 2) 5))
(defsubst vm-forwarded-flag (message) (aref (aref message 2) 6))
(defsubst vm-edited-flag (message) (aref (aref message 2) 7))
(defsubst vm-redistributed-flag (message) (aref (aref message 2) 8))
(defsubst vm-flagged-flag (message) (aref (aref message 2) 9))
(defsubst vm-folded-flag (message) (aref (aref message 2) 10))
(defsubst vm-watched-flag (message) (aref (aref message 2) 11))
(defsubst vm-ignored-flag (message) (aref (aref message 2) 12))
(defsubst vm-read-receipt-flag (message) (aref (aref message 2) 13))
(defsubst vm-read-receipt-sent-flag (message) (aref (aref message 2) 14))
(defsubst vm-attachments-flag (message) (aref (aref message 2) 15))

;; message cached data
(defconst vm-cached-data-vector-length 26)
(defconst vm-cached-data-fields
  [:byte-count :weekday :monthday :month :year :hour :zone
	       :full-name :from :message-id :line-count :subject
	       :vheaders-regexp :to :to-names :month-number
	       :sortable-datestring :sortable-subject 
	       :summary :parent :references
	       :body-to-be-discarded 
	       :body-to-be-retrieved
	       :uid :imap-uid-validity :spam-score])
;; message size in bytes (as a string)
(defsubst vm-byte-count-of (message) (aref (aref message 3) 0))
;; weekday sent
(defsubst vm-weekday-of (message) (aref (aref message 3) 1))
;; month day
(defsubst vm-monthday-of (message) (aref (aref message 3) 2))
;; month sent
(defsubst vm-month-of (message) (aref (aref message 3) 3))
;; year sent
(defsubst vm-year-of (message) (aref (aref message 3) 4))
;; hour sent
(defsubst vm-hour-of (message) (aref (aref message 3) 5))
;; timezone
(defsubst vm-zone-of (message) (aref (aref message 3) 6))
;; message author's full name (Full-Name: or gouged from From:)
(defsubst vm-full-name-of (message) (aref (aref message 3) 7))
;; message author address (gouged from From:)
(defsubst vm-from-of (message) (aref (aref message 3) 8))
;; message ID (Message-Id:)
(defsubst vm-message-id-of (message) (aref (aref message 3) 9))
;; number of lines in message (as a string)
(defsubst vm-line-count-of (message) (aref (aref message 3) 10))
;; message subject (Subject:)
(defsubst vm-subject-of (message) (aref (aref message 3) 11))
;; Regexp that can be used to find the start of the already ordered headers.
(defsubst vm-vheaders-regexp-of (message)
  (aref (aref message 3) 12))
;; Addresses of recipients in a comma separated list
(defsubst vm-to-of (message) (aref (aref message 3) 13))
;; Full names of recipients in a comma separated list.  Addresses if
;; full names not available.
(defsubst vm-to-names-of (message) (aref (aref message 3) 14))
;; numeric month sent
(defsubst vm-month-number-of (message) (aref (aref message 3) 15))
;; sortable date string (used for easy sorting, naturally)
(defsubst vm-sortable-datestring-of (message)
  (aref (aref message 3) 16))
;; sortable subject, re: garbage removed
(defsubst vm-sortable-subject-of (message)
  (aref (aref message 3) 17))
;; tokenized summary entry
(defsubst vm-summary-of (message)
  (aref (aref message 3) 18))
;; parent of this message, as determined by threading
(defsubst vm-parent-of (message)
  (aref (aref message 3) 19))
;; message IDs parsed from References header
(defsubst vm-references-of (message)
  (aref (aref message 3) 20))
;; have we retrieved the headers of this message?
;; only valid for remote folder access methods
;; USR: changed the name to vm-headers-to-be-retrieved-of because all the
;; VM folders in the world already have nil's written in this field. 
;; USR: changed it again to vm-body-to-be-discarded-of to allow for
;; fetched messages to be discarded before save.  2010-06-08
(defsubst vm-headers-to-be-retrieved-of (message)
  nil)
(defsubst vm-body-to-be-discarded-of (message)
  (aref (aref message 3) 21))
;; have we retrieved the body of this message?
;; only valid for remote folder access methods
;; USR: changed the name to vm-body-to-be-retrieved-of because all the
;; VM folders in the world already have nil's written in this field. 
(defsubst vm-body-to-be-retrieved-of (message)
  (aref (aref message 3) 22))
(defsubst vm-body-retrieved-of (message)
  (null (aref (aref message 3) 22)))
;; pop UIDL value for message
(defsubst vm-pop-uidl-of (message)
  (aref (aref message 3) 23))
;; imap UID value for message (shares same slot as pop-uidl-of)
(defsubst vm-imap-uid-of (message)
  (aref (aref message 3) 23))
;; imap UIDVALIDITY value for message
(defsubst vm-imap-uid-validity-of (message)
  (aref (aref message 3) 24))
(defsubst vm-spam-score-of (message)
  (aref (aref message 3) 25))

;; extra data shared by virtual messages if vm-virtual-mirror is non-nil
(defconst vm-mirror-data-vector-length 6)
(defconst vm-mirror-data-fields
  [:edit-buffer :virtual-messages-sym :stuff-flag :labels
  :label-string :attribute-modflag])
;; if message is being edited, this is the buffer being used.
(defsubst vm-edit-buffer-of (message) (aref (aref message 4) 0))
;; list of virtual messages mirroring the underlying real message
(defsubst vm-virtual-messages-of (message)
  (symbol-value (aref (aref message 4) 1)))
;; nil if all attribute changes have been stuffed into the folder buffer
(defsubst vm-stuff-flag-of (message) (aref (aref message 4) 2))
;; list of labels attached to this message
(defsubst vm-labels-of (message) (aref (aref message 4) 3))
;; comma list of labels
(defsubst vm-label-string-of (message) (aref (aref message 4) 4))
;; attribute modification flag for this message
;; non-nil if attributes need to be saved
(defsubst vm-attribute-modflag-of (message) (aref (aref message 4) 5))

(defsubst vm-set-start-of (message start)
  (aset (aref message 0) 0 start))
(defsubst vm-set-headers-of (message h)
  (aset (aref message 0) 1 h))
(defsubst vm-set-vheaders-of (message vh)
  (aset (aref message 0) 2 vh))
(defsubst vm-set-text-of (message text)
  (aset (aref message 0) 3 text))
(defsubst vm-set-text-end-of (message text)
  (aset (aref message 0) 4 text))
(defsubst vm-set-end-of (message end)
  (aset (aref message 0) 5 end))
(defsubst vm-set-number-of (message n)
  (aset (aref message 1) 0 n))
(defsubst vm-set-padded-number-of (message n)
  (aset (aref message 1) 1 n))
(defsubst vm-set-mark-of (message val)
  (aset (aref message 1) 2 val))
(defsubst vm-set-su-start-of (message pos)
  (aset (aref message 1) 3 pos))
(defsubst vm-set-su-end-of (message pos)
  (aset (aref message 1) 4 pos))
(defsubst vm-set-real-message-sym-of (message sym)
  (aset (aref message 1) 5 sym))
(defsubst vm-set-reverse-link-of (message link)
  (set (aref (aref message 1) 6) link))
(defsubst vm-set-reverse-link-sym-of (message sym)
  (aset (aref message 1) 6 sym))
(defsubst vm-set-message-type-of (message type)
  (aset (aref message 1) 7 type))
(defsubst vm-set-message-id-number-of (message number)
  (aset (aref message 1) 8 number))
(defsubst vm-set-buffer-of (message buffer)
  (aset (aref message 1) 9 buffer))
(defsubst vm-set-thread-indentation-of (message val)
  (aset (aref message 1) 10 val))
(defsubst vm-set-thread-list-of (message list)
  (aset (aref message 1) 11 list))
(defsubst vm-set-babyl-frob-flag-of (message flag)
  (aset (aref message 1) 12 flag))
(defsubst vm-set-saved-virtual-attributes-of (message attrs)
  (aset (aref message 1) 13 attrs))
(defsubst vm-set-saved-virtual-mirror-data-of (message data)
  (aset (aref message 1) 14 data))
(defsubst vm-set-virtual-summary-of (message summ)
  (aset (aref message 1) 15 summ))
(defsubst vm-set-mime-layout-of (message layout)
  (aset (aref message 1) 16 layout))
(defsubst vm-set-mime-encoded-header-flag-of (message flag)
  (aset (aref message 1) 17 flag))
(defsubst vm-set-su-summary-mouse-track-overlay-of (message overlay)
  (aset (aref message 1) 18 overlay))
(defsubst vm-set-message-access-method-of (message method)
  (aset (aref message 1) 19 method))
(defsubst vm-set-thread-subtree-of (message list)
  (aset (aref message 1) 20 list))
(defsubst vm-set-mirrored-message-sym-of (message sym)
  (aset (aref message 1) 21 sym))
(defsubst vm-set-thread-indentation-offset-of (message offset)
  (aset (aref message 1) 22 offset))

;; The other routines in attributes group are part of the undo system.
(defun vm-set-edited-flag-of (message flag)
  (aset (aref message 2) 7 flag)
  (vm-mark-for-summary-update message)
  (if (eq vm-flush-interval t)
      (vm-stuff-virtual-message-data message)
    (vm-set-stuff-flag-of message t))
  (unless (buffer-modified-p)
    (vm-mark-folder-modified-p (current-buffer)))
  (vm-clear-modification-flag-undos))
(defsubst vm-set-byte-count-of (message count)
  (aset (aref message 3) 0 count))
(defsubst vm-set-weekday-of (message val)
  (aset (aref message 3) 1 val))
(defsubst vm-set-monthday-of (message val)
  (aset (aref message 3) 2 val))
(defsubst vm-set-month-of (message val)
  (aset (aref message 3) 3 val))
(defsubst vm-set-year-of (message val)
  (aset (aref message 3) 4 val))
(defsubst vm-set-hour-of (message val)
  (aset (aref message 3) 5 val))
(defsubst vm-set-zone-of (message val)
  (aset (aref message 3) 6 val))
(defsubst vm-set-full-name-of (message author)
  (aset (aref message 3) 7 author))
(defsubst vm-set-from-of (message author)
  (aset (aref message 3) 8 author))
(defsubst vm-set-message-id-of (message id)
  (aset (aref message 3) 9 id))
(defsubst vm-set-line-count-of (message count)
  (aset (aref message 3) 10 count))
(defsubst vm-set-subject-of (message subject)
  (aset (aref message 3) 11 subject))
(defsubst vm-set-vheaders-regexp-of (message regexp)
  (aset (aref message 3) 12 regexp))
(defsubst vm-set-to-of (message recips)
  (aset (aref message 3) 13 recips))
(defsubst vm-set-to-names-of (message recips)
  (aset (aref message 3) 14 recips))
(defsubst vm-set-month-number-of (message val)
  (aset (aref message 3) 15 val))
(defsubst vm-set-sortable-datestring-of (message val)
  (aset (aref message 3) 16 val))
(defsubst vm-set-sortable-subject-of (message val)
  (aset (aref message 3) 17 val))
(defsubst vm-set-summary-of (message val)
  (aset (aref message 3) 18 val))
(defsubst vm-set-parent-of (message val)
  (aset (aref message 3) 19 val))
(defsubst vm-set-references-of (message val)
  (aset (aref message 3) 20 val))
(defsubst vm-set-headers-to-be-retrieved-of (message val)
  nil)
(defsubst vm-set-body-to-be-discarded-of (message val)
  (aset (aref message 3) 21 val))
(defsubst vm-set-body-to-be-retrieved-of (message val)
  (aset (aref message 3) 22 val))
(defsubst vm-set-pop-uidl-of (message val)
  (aset (aref message 3) 23 val))
(defsubst vm-set-imap-uid-of (message val)
  (aset (aref message 3) 23 val))
(defsubst vm-set-imap-uid-validity-of (message val)
  (aset (aref message 3) 24 val))
(defsubst vm-set-spam-score-of (message val)
  (aset (aref message 3) 25 val))
(defsubst vm-set-edit-buffer-of (message buf)
  (aset (aref message 4) 0 buf))
(defsubst vm-set-virtual-messages-of (message list)
  (set (aref (aref message 4) 1) list))
(defsubst vm-set-virtual-messages-sym-of (message sym)
  (aset (aref message 4) 1 sym))
(defsubst vm-set-stuff-flag-of (message val)
  (aset (aref message 4) 2 val))
(defsubst vm-set-labels-of (message labels)
  (aset (aref message 4) 3 labels))
(defsubst vm-set-label-string-of (message string)
  (aset (aref message 4) 4 string))
(defsubst vm-set-attribute-modflag-of (message flag)
  (aset (aref message 4) 5 flag))

(defun vm-mime-encode-words-in-cache-vector (vector)
  (let ((new-vector (make-vector vm-cached-data-vector-length nil)))
    ;; Encode the fields of the original cache-vector as necessary.
    ;; Some of the fields have been mime-decoded with text properties.
    ;; And, some haven't.
    ;; This is a mess.
    ;; Others probably don't need any mime-encoding, but we encode
    ;; them anyway for safety.

    ;; byte-count
    (aset new-vector 0 (aref vector 0))
    ;; weekday
    (aset new-vector 1 (vm-mime-encode-words-in-string (aref vector 1)))
    ;; monthday
    (aset new-vector 2 (vm-mime-encode-words-in-string (aref vector 2)))
    ;; month
    (aset new-vector 3 (vm-mime-encode-words-in-string (aref vector 3)))
    ;; year
    (aset new-vector 4 (vm-mime-encode-words-in-string (aref vector 4)))
    ;; hour
    (aset new-vector 5 (vm-mime-encode-words-in-string (aref vector 5)))
    ;; zone
    (aset new-vector 6 (vm-mime-encode-words-in-string (aref vector 6)))
    ;; full-name
    (aset new-vector 7 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 7)))
    ;; from
    (aset new-vector 8 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 8)))
    ;; message-id
    (aset new-vector 9 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 9)))
    ;; line-count
    (aset new-vector 10 (vm-mime-encode-words-in-string (aref vector 10)))
    ;; subject
    (aset new-vector 11 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 11)))
    ;; vheaders-regexp
    (aset new-vector 12 (vm-mime-encode-words-in-string (aref vector 12)))
    ;; to
    (aset new-vector 13 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 13)))
    ;; to-names
    (aset new-vector 14 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 14)))
    ;; month-number
    (aset new-vector 15 
	  (vm-mime-encode-words-in-string (aref vector 15)))
    ;; sortable-date-string
    (aset new-vector 16 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 16)))
    ;; sortable-subject
    (aset new-vector 17 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 17)))
    ;; summary
    (aset new-vector 18 
	  (vm-reencode-mime-encoded-words-in-tokenized-summary 
	   (aref vector 18)))
    ;; parent
    (aset new-vector 19 
	  (vm-reencode-mime-encoded-words-in-string (aref vector 19)))
    ;; references
    (aset new-vector 20 
	  (mapcar (function vm-reencode-mime-encoded-words-in-string)
		  (aref vector 20)))
    ;; body-to-be-discarded (formerly headers-to-be-retrieved)
    (aset new-vector 21 (aref vector 21))
    ;; body-to-be-retrieved
    (aset new-vector 22 (aref vector 22))
    ;; pop-uidl or imap-uid
    (aset new-vector 23 (vm-mime-encode-words-in-string (aref vector 23)))
    ;; imap-uid-validity
    (aset new-vector 24 (vm-mime-encode-words-in-string (aref vector 24)))
    ;; spam-score is a number.  nothing to do

    new-vector))


(defun vm-make-message ()
  "Create a new blank message struct."
  (let ((mvec (make-vector 5 nil))
	sym)
    (vm-set-softdata-of mvec (make-vector vm-softdata-vector-length nil))
    (vm-set-location-data-of
     mvec (make-vector vm-location-data-vector-length nil))
    (vm-set-mirror-data-of 
     mvec (make-vector vm-mirror-data-vector-length nil))
    (vm-set-message-id-number-of mvec (int-to-string vm-message-id-number))
    (vm-increment vm-message-id-number)
    (vm-set-buffer-of mvec (current-buffer))
    ;; We use an uninterned symbol here as a level of indirection
    ;; from a purely self-referential structure.  This is
    ;; necessary so that Emacs debugger can be used on this
    ;; program.
    (setq sym (make-symbol "<<>>"))
    (set sym mvec)
    (vm-set-real-message-sym-of mvec sym)
    (vm-set-mirrored-message-sym-of mvec sym)
    ;; Another uninterned symbol for the virtual messages list.
    (setq sym (make-symbol "<v>"))
    (set sym nil)
    (vm-set-virtual-messages-sym-of mvec sym)
    ;; Another uninterned symbol for the reverse link
    ;; into the message list.
    (setq sym (make-symbol "<--"))
    (vm-set-reverse-link-sym-of mvec sym)
    mvec ))

(defun vm-find-and-set-text-of (m)
  (save-excursion
    (set-buffer (vm-buffer-of m))
    (save-restriction
      (widen)
      (goto-char (vm-headers-of m))
      (search-forward "\n\n" (vm-text-end-of m) 0)
      (vm-set-text-of m (point-marker)))))

(defsubst vm-virtual-message-p (m)
  (not (eq m (vm-real-message-of m))))

(defun vm-update-virtual-messages (m &key message-changing)
  "Update all the virtual messages of M to reflect the changes made to
the headers/body of M."
  (save-excursion
    (mapc (lambda (v-m)
	    (vm-set-mime-layout-of v-m nil)
	    (vm-set-mime-encoded-header-flag-of v-m nil)
	    (vm-set-line-count-of v-m nil)
	    (when (buffer-name (vm-buffer-of v-m))
	      (set-buffer (vm-buffer-of v-m))
	      (if (and vm-presentation-buffer
		       (eq (car vm-message-pointer) v-m))
		  (save-excursion (vm-present-current-message)))
	      (when (vectorp vm-thread-obarray)
		;; this was changed from v-m to m in revision 1148, but it
		;; doesn't make sense. USR, 2011-04-28 
		(vm-unthread-message v-m :message-changing message-changing)
		(vm-build-threads (list v-m)))
	      ;; (if vm-summary-show-threads
	      ;;     (intern (buffer-name) buffers-needing-thread-sort))
	      ))
	  (vm-virtual-messages-of m))))

(defun vm-pp-message (m)
  (pp
   (vector 
     ':location-data 
     (vm-zip-vectors vm-location-data-fields (vm-location-data-of m))
     ':softdata
     (vm-zip-vectors vm-softdata-fields (vm-softdata-of m))
     ':attributes
     (vm-zip-vectors vm-attributes-fields (vm-attributes-of m))
     ':cached-data
     (vm-zip-vectors vm-cached-data-fields (vm-cached-data-of m))
     ':mirror-data
     (vm-zip-vectors vm-mirror-data-fields (vm-mirror-data-of m))))
  nil)

;;; vm-message.el ends here
