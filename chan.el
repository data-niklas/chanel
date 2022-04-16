;;; chan.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Niklas Loeser
;;
;; Author: Niklas Loeser <niklas@4loeser.net>
;; Maintainer: Niklas Loeser <niklas@4loeser.net>
;; Created: April 15, 2022
;; Modified: April 15, 2022
;; Version: 0.0.1
;; Keywords: hypermedia, multimedia
;; Homepage: https://github.com/data-niklas/chan
;; Package-Requires: ((dash "2.19.1") (emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'dash)         ; Parsing trees
(require 'easymenu)     ; Chanel menu definition


;; Definitions
(defgroup chanel nil
  "Imageboard viewer mainly for 4chan."
  :group 'hypermedia
  :group 'multimedia
  :prefix "chanel-"
  :link '(url-link :tag "Github" "https://github.com/data-niklas/chanel"))


(defconst chanel-imageboard-list-url-nsfw "https://www.4chan.org/frames_navigation?disclaimer=accept&wsonly=no"
  "URL used to display a list of all 4chan boards.")
(defconst chanel-imageboard-list-url-sfw "https://www.4chan.org/frames_navigation?disclaimer=accept&wsonly=yes"
  "URL used to display a list of all sfw 4chan boards.")

(defcustom chanel-allow-nsfw
  nil
  "Toggles if nsfw imageboards may be shown."
  :group 'chanel
  :type 'boolean)


;; Logic
(defun chanel-parse-page (url)
  "Parse a website into a libxml tree. The website is passed as an URL."
  (unless url
    (error "Couldn't find URL"))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (libxml-parse-html-region (point-min) (point-max)))
      (kill-buffer buffer))))


(defun chanel-imageboard-list-url ()
  "Determine the correct URL to access the list of all imageboards.
Respects chanel-allow-nsfw."
  (if chanel-allow-nsfw
      chanel-imageboard-list-url-nsfw
    chanel-imageboard-list-url-sfw))

(defun chanel-imageboard-extract-links (url)
  "Extracts all imageboard links from URL."
  (dom-by-tag
   (dom-by-id
    (chanel-parse-page url)
    "img")
   'a))

(defun chanel-imageboard-url-from-a-node (node)
  "Return a URL to the imageboard from a 'a' NODE."
  (concat "https:" (dom-attr node 'href)))

(defun chanel-imageboard-shorthand-from-a-node (node)
  "Return the imageboard shorthand from a 'a' NODE."
  (substring (dom-attr node 'href) 18))

(defun chanel-imageboard-name-from-a-node (node)
  "Return the imageboard name from a 'a' NODE."
  (dom-attr node 'title))


(defun chanel-imageboard-make-records-from-links (links)
  "Create imageboard records from LINKS."
  (--map
   (record 'imageboard
           (chanel-imageboard-name-from-a-node it)
           (chanel-imageboard-shorthand-from-a-node it)
           (chanel-imageboard-url-from-a-node it))
   links))

(defun chanel-imageboard-list ()
  "Return a list of all imageboards.
Each imageboard is represented as a record.
chanel-allow-nsfw will be respected."
  (chanel-imageboard-make-records-from-links
   (chanel-imageboard-extract-links
    (chanel-imageboard-list-url))))




;; UI
(defun chanel-merge-lines (lines)
  "Merge all LINES into one big chunk of text."
  (--reduce (format "%s\n%s" acc it) lines))

(defun chanel-imageboard-display-record (imageboard)
  "Return displayable text if IMAGEBOARD."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down-mouse-1>")
      `(lambda() (interactive) (message-box ,(aref imageboard 3))))
    (propertize
     (format "%-8s%s" (aref imageboard 2) (aref imageboard 1))
     'mouse-face 'highlight
     'keymap map)))

(defun chanel-imageboard-display-records (imageboards)
  "Return displayable text of IMAGEBOARDS."
  (chanel-merge-lines (--map (chanel-imageboard-display-record it) imageboards)))


;; Navigation
(defun chanel-entry-next ()
  "Focus the next entry in the current buffer."
  (interactive)
  (message "To be done."))

(defun chanel-entry-previous ()
  "Focus the previous entry in the current buffer."
  (interactive)
  (message "To be done."))


(defun chanel ()
  "Open the board list."
  (interactive)
  (pop-to-buffer "imageboard list - chanel")
  ;; (chanel-mode)
  (erase-buffer)
  (insert (chanel-imageboard-display-records (chanel-imageboard-list))))



(provide 'chan)
;;; chan.el ends here
