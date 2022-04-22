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
;; Package-Requires: ((dash "2.19.1") (emacs "27.1"))
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

(defconst chanel-board-api-url "https://a.4cdn.org/boards.json"
  "URL used to retrieve a list of all boards.")

(defconst chanel-board-threads-api-url-template "https://a.4cdn.org/%s/catalog.json"
  "URL template to retrieve list of threads or a board.")

(defconst chanel-board-thread-api-url-template "https://a.4cdn.org/%s/thread/%s.json"
  "URL template to retrieve a single thread.")

(defconst chanel-media-api-url-template "https://i.4cdn.org/%s/%d%s"
  "URL template to retrieve media.")

(defcustom chanel-allow-nsfw
  nil
  "Toggles if nsfw imageboards may be shown."
  :group 'chanel
  :type 'boolean)

(defcustom chanel-post-image-preview
  nil
  "Toggles if previews or full images are used."
  :group 'chanel
  :type 'boolean)

;; Mode
(defvar chanel-mode-hook nil)
(defvar chanel-mode-map (make-sparse-keymap)
  "Keymap for chanel major mode.")
(define-key chanel-mode-map "n" 'chanel-next-image)
(define-key chanel-mode-map "p" 'chanel-previous-image)


(define-derived-mode chanel-mode nil "Chanel"
  "4chan imageboard mode."
  :group 'chanel :keymap chanel-mode-map)

;; Logic
(defun chanel-retrieve-json-synchronous (url)
  "Parse a JSON website into elisp.
The website is passed as an URL."
  (unless url
    (error "Couldn't find URL"))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char url-http-end-of-headers)
          (json-parse-buffer :array-type 'list))
      (kill-buffer buffer))))

(defun chanel-retrieve-json (url callback &optional args)
  "Retrieve JSON from URL and parse it into elisp.
The result is passed to CALLBACK.
ARGS are also passed to the CALLBACK."
  (url-retrieve
   url
   (lambda
     (status &rest args)
     (let ((buffer (current-buffer)))
       (goto-char url-http-end-of-headers)
       (apply callback
              (json-parse-buffer :array-type 'list)
              args)
       (kill-buffer buffer)))
   args))

(defun chanel-retrieve-image (url callback &optional args)
  "Retrieve image from URL and parse it into an elisp image.
The result is passed to CALLBACK.
ARGS are also passed to the CALLBACK."
  (url-retrieve
   url
   (lambda
     (status &rest args)
     (let ((buffer (current-buffer)))
       (goto-char (point-min))
       (search-forward "\n\n")
       (apply callback
              (create-image
               (buffer-substring (point) (point-max))
               nil
               t)
              args)
       (kill-buffer buffer)))
   args))


;; Imageboard functions
(defun chanel-imageboard-filter-nsfw (boards)
  "Filters nsfw BOARDS depending on `chanel-allow-nsfw'."
  (if chanel-allow-nsfw
      boards
    (--filter
     (= 1 (gethash "ws_board" it))
     boards)))

(defun chanel-imageboard-list ()
  "Retrieve a list of all imageboards.
Each imageboard is represented as a record."
  (chanel-imageboard-filter-nsfw
   (gethash
    "boards"
    (chanel-retrieve-json-synchronous chanel-board-api-url))))


;; Thread functions
(defun chanel-thread-list (imageboard)
  "Retrieve a list of all threads of IMAGEBOARD."
  (-flatten-n 1
              (--map
               (gethash "threads" it)
               (chanel-retrieve-json-synchronous
                (format chanel-board-threads-api-url-template imageboard)))))


;; Post functions
(defun chanel-post-list (board thread)
  "Retrieve a list of all posts of THREAD of BOARD."
  (gethash
   "posts"
   (chanel-retrieve-json-synchronous
    (format chanel-board-thread-api-url-template board thread))))


(defun chanel-decode-html-entities (text)
  "Decodes HTML entities in TEXT."
  (replace-regexp-in-string
   "&#[0-9]*;"
   (lambda (match)
     (format "%c" (string-to-number (substring match 2 -1))))
   text))

;; UI
;; Utils
(defun chanel-propertize-clickable (text cbleft cbright)
  "Propertize TEXT.
Make TEXT clickable and call CBLEFT when left-clicked and CBRIGHT when right-clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down-mouse-1>") cbleft)
    (define-key map (kbd "<down-mouse-3>") cbright)
    (propertize
     text
     'mouse-face 'highlight
     'keymap map)))

(defun chanel-insert-image-async (url)
  "Insert an image, loaded from URL, at the current point async."
  (let ((mark (make-marker)))
    (set-marker mark (point))
    (chanel-retrieve-image
     url
     (lambda (image buffer)
       (with-current-buffer buffer
         (save-excursion
           (goto-char (marker-position mark))
           (insert-image image))))
     `(,(current-buffer)))))

(defun chanel-clean-text (text)
  "Clean TEXT and unescape HTML text."
  (chanel-decode-html-entities text))


;; Post
(defun chanel-image-url-from-post (post board)
  "Return image url from POST object of BOARD."
  (format chanel-media-api-url-template board
          (gethash "tim" post)
          (gethash "ext" post)))

(defun chanel-image-preview-url-from-post (post board)
  "Return image preview from POST object of BOARD."
  (format chanel-media-api-url-template board
          (gethash "tim" post)
          "s.jpg"))

(defun chanel-post-has-image (post)
  "Return t if POST has an attached image."
  (gethash "tim" post))

(defun chanel-post-subject (post)
  "Return a POST's subject."
  (chanel-clean-text (or
                      (gethash "sub" post)
                      (gethash "com" post))))

(defun chanel-post-comment (post)
  "Return a POST's comment."
  (chanel-clean-text (or
                      (gethash "com" post)
                      (gethash "sub" post))))


;; Imageboard
(defun chanel-imageboard-display-imageboard (imageboard)
  "Display a single IMAGEBOARD."
  (insert
   (chanel-propertize-clickable
    (format
     "%-8s%s"
     (format "/%s/" (gethash "board" imageboard))
     (gethash "title" imageboard))
    `(lambda() (interactive) (chanel-threads ,(gethash "board" imageboard)))
    nil))
  (insert "\n"))

(defun chanel-imageboard-display-imageboards (imageboards)
  "Display all IMAGEBOARDS."
  (--each
      imageboards
    (chanel-imageboard-display-imageboard it)))


;; Threads
(defun chanel-threads-display-thread (thread board)
  "Display THREAD.
Uses BOARD to display THREAD."
  (if (chanel-post-has-image thread)
      (chanel-insert-image-async (chanel-image-preview-url-from-post thread board)))
  (insert "\n")
  (insert
   (chanel-propertize-clickable
    (chanel-post-subject thread)
    `(lambda()(interactive) (chanel-thread ,board ,(gethash "no" thread)))
    `(lambda()(interactive) (chanel-thread-images ,board ,(gethash "no" thread)))))
  (insert "\n\n"))

(defun chanel-threads-display-threads (threads board)
  "Display all THREADS.
Uses BOARD to display THREADS."
  (--each
      threads
    (chanel-threads-display-thread it board)))


;; Posts
(defun chanel-posts-display-post (post board)
  "Display a single POST of BOARD."
  (chanel-posts-display-post-image post board)
  (insert (chanel-post-comment post))
  (insert "\n\n"))

(defun chanel-posts-display-posts (posts board)
  "Display all POSTS of THREAD of BOARD."
  (--each
      posts
    (chanel-posts-display-post it board)))


(defun chanel-posts-display-post-image (post board)
  "Display a single POST image of BOARD."
  (if (chanel-post-has-image post)
      (chanel-insert-image-async
       (if chanel-post-image-preview
           (chanel-image-preview-url-from-post post board)
         (chanel-image-url-from-post post board))))
  (insert "\n"))

(defun chanel-posts-display-posts-images (posts board)
  "Display all POSTS of THREAD of BOARD.
Only images will be displayed."
  (--each
      posts
    (chanel-posts-display-post-image it board)))


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
  (pop-to-buffer-same-window "imageboard list - chanel")
  (erase-buffer)
  (chanel-imageboard-display-imageboards (chanel-imageboard-list)))

(defun chanel-threads (board)
  "Display a list of all threads for BOARD."
  (interactive "s")
  (pop-to-buffer-same-window (format "/%s/" board))
  (erase-buffer)
  (chanel-threads-display-threads (chanel-thread-list board) board)
  (goto-char (point-min))
  (chanel-mode))

(defun chanel-thread (board thread)
  "Display a single THREAD of BOARD."
  (interactive "s\ns")
  (pop-to-buffer-same-window (format "/%s/%s" board thread))
  (erase-buffer)
  (chanel-posts-display-posts
   (chanel-post-list board thread)
   board)
  (goto-char (point-min))
  (chanel-mode))

(defun chanel-thread-images (board thread)
  "Display all images of THREAD of BOARD."
  (interactive "s\ns")
  (pop-to-buffer (format "/%s/%s" board thread))
  (erase-buffer)
  (chanel-posts-display-posts-images
   (chanel-post-list board thread)
   board)
  (goto-char (point-min))
  (chanel-mode))

(defun chanel-next-image ()
  "Jump to the next image."
  (interactive)
  (text-property-search-forward 'display nil nil))

(defun chanel-previous-image ()
  "Jump to the previous image."
  (interactive)
  (text-property-search-backward 'display nil nil))

(provide 'chan)
;;; chan.el ends here
