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
;; Package-Requires: ((dash "2.19.1") (emacs "24.3"))
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


(defcustom chanel-allow-nsfw
  nil
  "Toggles if nsfw imageboards may be shown."
  :group 'chanel
  :type 'boolean)

(defcustom chanel-imageboard-list
  '("https://4chan.org/")
  "A list of all imageboard sites.
Each entry is the URL to the imageboard."
  :group 'chanel
  :type '(repeat string))


;; Logik
(defun chanel-parse-page (url)
  "Parse a website into a libxml tree. The website is passed as an URL."
  (unless url
    (error "Couldn't find URL"))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (libxml-parse-html-region (point-min) (point-max)))
      (kill-buffer buffer))))



;; UI


;; Navigation
(defun chanel-entry-next ()
  "Focus the next entry in the current buffer."
  (interactive)
  (message "To be done."))

(defun chanel-entry-previous ()
  "Focus the previous entry in the current buffer."
  (interactive)
  (message "To be done."))

(defun chanel-page-next ()
  "Display the next page."
  (interactive)
  (message "To be done."))

(defun chanel-page-previous ()
  "Display the previous page."
  (interactive)
  (message "To be done."))

(defun chanel-page-by-number (page)
  "Display the page given by PAGE."
  (interactive "d")
  (message "To be done."))

(defun chanel ()
  "Open the imageboard list."
  (interactive)
  (pop-to-buffer "chanel imageboard list")
  ;; (chanel-mode)
  (erase-buffer)
  (insert "Imageboard list\n\n")
  (insert (--reduce (format "%s\n%s" acc it) chanel-imageboard-list)))



(provide 'chan)
;;; chan.el ends here
