;;; iimg.el --- Inline image      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package provides three functionalities:
;;  1. Embedding images into text files by encoding them to base64
;;     strings.
;;  2. Rendering embedded images.
;;  3. Control the size of the displayed image.
;;
;; Why embed the image? This way everything is in a single file and I
;; feel safe.
;;
;;;; To enable:
;;
;;     M-x iimg-minor-mode RET
;;
;;;; To insert an image:
;;
;; Drag and drop the image or use `iimg-insert'. Emacs will prompt for
;; a caption/name for the image. If you don’t enter anything, Emacs
;; generates a unique string as the fallback.
;;
;;;; To resize an image:
;;
;; Type s on an image or use `iimg-resize'. In the minibuffer, type in
;; the specification in the format of SIDE UNIT AMOUNT.
;;
;; SIDE can be width or height.
;; UNIT can be char or pixel.
;; AMOUNT can be a float or a integer.
;;
;; For example, “width char 40” means 40 characters wide. If AMOUNT is
;; a floating point number like 0.5, it is interpreted as a percentage
;; to the width/height of the window and UNIT is ignored.
;;
;; The default width is (width char 70).
;;
;;;; To toggle thumbnail display:
;;
;; Type t on an image or use `iimg-toggle-thumbnail'.
;;
;; When you insert an image, the image appears at point is just a
;; link, the actual base64 data is appended at the end of the file. I
;; separate link and data because that way readers incapable of
;; rendering inline images can still view the rest of the document
;; without problems.
;;
;; To protect the image data, iimg marks them read-only, to delete
;; the data, select a region and use `iimg-force-delete'.
;;
;; I didn’t bother to write unfontification function.

;;; Developer
;;
;; IIMG-DATA := ({iimg-data (:name STRING :data STRING)})
;; IIMG-LINK := ({iimg-link (:name STRING :thumbnail BOOL :size SIZE)})
;; SIZE  := (SIDE UNIT NUMBER)
;; SIDE  := width | height
;; UNIT  := char | pixel
;;
;; How does iimg work:
;;  1. Scan through the file for iimg data, load images into
;;     `iimg--data-alist'.
;;  2. In jit-lock, render iimg links to images.
;;  3. When inserting a new image, update `iimg--data-alist',
;;     insert the data at the end of the file, and insert the link
;;     at point.
;;
;; `iimg--data-alist' is always up to date: any image in the file are
;; in the alist.

;;; Code:
;;

;;; Variables

(defvar-local iimg--data-alist nil
  "An alist of (NAME . IMAGE-DATA).
NAME (string) is the name of the image.
IMAGE-DATA is the image binary data.")

(defvar iimg--data-regexp (rx (seq "({iimg-data "
                                   (group (+? anything))
                                   "})"))
  "Regular expression for inline image data.
The first group is the plist containing data.")

(defvar iimg--link-regexp (rx (seq "({iimg-link "
                                   (group (+? anything))
                                   "})"))
  "Regular expression for inline image link.
The first group is the plist containing data.")

(defsubst iimg--format (type plist)
  "Return formatted iimg link.
PLIST is the plist part of the link, should be a plist.
TYPE should be either 'link or 'data."
  (format "({iimg-%s %s})" type (prin1-to-string plist)))

(defvar iimg--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "t" #'iimg-toggle-thumbnail)
    (define-key map "s" #'iimg-resize)
    (define-key map "d" #'iimg-delete-image-at-point)
    map)
  "Keymap used on images.")

;;; Loading and rendering

(defun iimg--check-integrity (beg end)
  "Unfontify damaged links from BEG to END."
  (save-excursion
    (goto-char beg)
    (let (match)
      ;; Search for regiones that has text-prop iimg.
      (while (and (setq match (text-property-search-forward 'iimg t t))
                  (<= (point) end))
        (let* ((beg (prop-match-beginning match))
               (end (prop-match-end match))
               (string (buffer-substring-no-properties beg end)))
          ;; If the text don’t match link regexp anymore, remove
          ;; the image display.
          (unless (string-match-p iimg--link-regexp string)
            (with-silent-modifications
              (put-text-property beg end 'display nil)
              (put-text-property beg end 'iimg nil)
              (put-text-property beg end 'keymap nil))))))))

(defun iimg--fontify (beg end)
  "Fontify embedded image links between BEG and END."
  (iimg--check-integrity beg end)
  ;; Fontify link.
  (goto-char beg)
  (while (re-search-forward iimg--link-regexp end t)
    ;; PROPS includes :name, :thumbnail, :size
    (let* ((props (read (buffer-substring-no-properties
                         (match-beginning 1) (match-end 1))))
           (name (plist-get props :name))
           (thumbnail (plist-get props :thumbnail))
           (size (plist-get props :size))
           (size-spec (if thumbnail
                          ;; TODO This thumbnail size should work in most
                          ;; cases, but can be improved.
                          (iimg--calculate-size '(width char 30))
                        (and size (iimg--calculate-size size))))
           ;; Below `iimg--data-of' calls `iimg--load-image'
           ;; which does regexp search, we save our match info
           ;; so it’s not messed up. I added `save-match-data'
           ;; in `iimg--load-image', but anyway.
           (beg (match-beginning 0))
           (end (match-end 0))
           (image (apply #'create-image
                         (iimg--data-of name) nil t size-spec)))
      (with-silent-modifications
        (put-text-property beg end 'display image)
        (put-text-property beg end 'keymap iimg--link-keymap)
        (put-text-property beg end 'iimg t)
        (put-text-property beg end 'rear-nonsticky
                           '(display keymap iimg)))))
  (cons 'jit-lock-response (cons beg end)))

(defun iimg--calculate-size (size)
  "Translate SIZE to an size that `create-image' recognizes.
IOW, (:width NUMBER) or (:height NUMBER), where NUMBER is in
pixels.
Calculation is done based on the current window."
  (pcase-let*
      ((`(,side ,unit ,amount) size)
       ;; Pixel width/height of a character.
       (char-pixel-len (pcase side
                         ('width (frame-char-width))
                         ('height (frame-char-height))
                         (_ (signal 'iimg-invalid-size size))))
       ;; Pixel wdith/height of the window
       (window-len (pcase side
                     ('width (window-width nil t))
                     ('height (window-height nil t))
                     (_ (signal 'iimg-invalid-size size))))
       ;; Pixel width/height of a character or pixel.
       (unit-len (pcase unit
                   ('char char-pixel-len)
                   ('pixel 1)
                   (_ (signal 'iimg-invalid-size size))))
       (len (pcase amount
              ;; This much char or pixels.
              ((pred integerp) (floor (* amount unit-len)))
              ;; This percent of the window width/height.
              ((pred floatp) (floor (* amount window-len)))
              (_ (signal 'iimg-invalid-size size)))))

    (pcase side
      ('width (list :width len))
      ('height (list :height len)))))

(defun iimg--load-image-data (beg end)
  "Load iimg data from BEG to END.
Look for iimg-data’s and store them into `iimg--data-alist'."
  ;; This could be called from within `iimg--fontify', and we
  ;; don’t want to mess up its match data.
  (save-match-data
    (save-excursion
      (goto-char beg)
      (while (re-search-forward iimg--data-regexp end t)
        (let* ((beg (match-beginning 1))
               (end (match-end 1))
               (props (read (buffer-substring-no-properties beg end)))
               (name (plist-get props :name))
               (base64-string (plist-get props :data))
               (image-data (base64-decode-string base64-string)))
          (setf (alist-get name iimg--data-alist nil t #'equal)
                image-data)
          ;; We fontify data here because data are usually to long
          ;; to be handled correctly by jit-lock.
          (with-silent-modifications
            (let ((beg (match-beginning 0))
                  (end (match-end 0)))
              (put-text-property
               beg end 'display (format "[iimg data of %s]" name))
              (put-text-property beg end 'read-only t)
              ;; This allows inserting after the data.
              (put-text-property beg end 'rear-nonsticky
                                 '(read-only display)))))))))

(defun iimg--data-of (name)
  "Get the image data of NAME (string)."
  (when (not iimg--data-alist)
    (iimg--load-image-data (point-min) (point-max)))
  (alist-get name iimg--data-alist nil nil #'equal))

;;; Inserting and modifying

(defun iimg-insert (file name)
  "Insert FILE at point as an inline image.
NAME is the name of the image, THUMBNAIL determines whether to
display the image as a thumbnail, SIZE determines the size of the
image. See Commentary for the format of NAME, THUMBNAIL, and SIZE."
  (interactive
   (list (expand-file-name (read-file-name "Image: "))
         (let ((name (read-string "Caption/name for the image: ")))
           (if (equal name "")
               (format-time-string "%s")
             name))))
  (let* ((data (with-temp-buffer
                 (insert-file-contents-literally file)
                 (base64-encode-region (point-min) (point-max))
                 ;; TODO Check for max image file size?
                 (buffer-string)))
         (data-string (iimg--format
                       'data (list :name name :data data)))
         (link-string (iimg--format
                       'link (list :name name :size '(width pixel 0.6)
                                   :ext (file-name-extension file)))))
    ;; Insert data.
    (save-excursion
      (goto-char (point-max))
      (let ((beg (point)))
        (insert "\n" data-string "\n")
        (iimg--load-image-data beg (point))))
    ;; Insert link.
    (insert link-string)))

(defun iimg--search-link-at-point ()
  "Search for iimg link at point.
If found, set match data accordingly and return t, if not, return nil."
  (let ((p (point)))
    (save-excursion
      (and (re-search-forward iimg--link-regexp nil t)
           (and (<= (match-beginning 0) p)
                (>= (match-end 0) p))
           t))))

(defun iimg--link-at-point ()
  "Return the data (plist) of the iimg link at point.
Return nil if not found."
  (when (iimg--search-link-at-point)
    (read (buffer-substring-no-properties
           (match-beginning 1) (match-end 1)))))

(defun iimg--set-link-at-point-refresh (props)
  "Set iimg link at point to PROPS, if there is any link.
Also refresh the image at point."
  (when (iimg--search-link-at-point)
    (save-excursion
      (let ((beg (match-beginning 0)))
        (goto-char beg)
        (delete-region beg (match-end 0))
        (insert (iimg--format 'link props))
        (iimg--fontify beg (point))))))

(defun iimg-resize ()
  "Resize the inline image at point."
  (interactive)
  (if-let ((img-props (iimg--link-at-point)))
      (let ((size (read
                   (format "(%s)"
                           (read-string
                            "width/height char/pixel amount: ")))))
        (setq img-props (plist-put img-props :size size))
        (iimg--set-link-at-point-refresh img-props))
    (user-error "There is no image at point")))

(defun iimg-toggle-thumbnail ()
  "Toggle thumbnail display for the image at point."
  (interactive)
  (if-let ((img-props (iimg--link-at-point)))
      (progn (setq img-props
                   (plist-put img-props :thumbnail
                              (not (plist-get img-props :thumbnail))))
             (iimg--set-link-at-point-refresh img-props))
    (user-error "There is no image at point")))

(defun iimg-delete-image-at-point ()
  "Delete the image at point."
  (interactive)
  (if (iimg--search-link-at-point)
      (delete-region (match-beginning 0) (match-end 0))
    (user-error "There is no image at point")))

(defun iimg-force-delete (beg end)
  "Force delete data between BEG and END."
  (interactive "r")
  (let ((inhibit-read-only t))
    (delete-region beg end)))

(defun iimg-export ()
  "Export image at point."
  (interactive)
  (if-let ((img-props (iimg--link-at-point)))
      (let ((path (concat (read-file-name "Export to (w/o extension): ")
                          (or (plist-get img-props :ext) ".png")))
            (data (iimg--data-of (plist-get img-props :name))))
        (when (file-exists-p path)
          (user-error "File exists, can’t export to it"))
        (when (not (file-writable-p path))
          (user-error "File not wraiteble, can’t export to it"))
        (with-temp-file path
          (insert data))
        (message "Exported to %s" path))
    (user-error "There is no image at point")))

;;; Minor mode

(define-minor-mode iimg-minor-mode
  "Display inline iamges."
  :lighter ""
  (if iimg-minor-mode
      (progn (jit-lock-register #'iimg--fontify)
             (setq-local dnd-protocol-alist
                         (cons '("^file:" . iimg-dnd-open-file)
                               dnd-protocol-alist)))
    (jit-lock-unregister #'iimg--fontify)
    (kill-local-variable 'dnd-protocol-alist))
  (jit-lock-refontify))

;;; Drag and drop

(defun iimg-dnd-open-file (uri _action)
  (let ((file (dnd-get-local-file-name uri t)))
    (if (and file (file-readable-p file))
        (iimg-insert
         file (let ((name (read-string "Caption/name for the image: ")))
                (if (equal name "")
                    (format-time-string "%s")
                  name))))))

(provide 'iimg)

;;; iimg.el ends here
