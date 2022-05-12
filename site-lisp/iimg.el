;;; iimg.el --- Inline image      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This package provides three functionalities:
;;  1. Embedding images into text files by encoding them to base64
;;     strings.
;;  2. Rendering embedded images.
;;  3. Controlling the size of the displayed image.
;;
;; Why embed the image? This way everything is in a single file and I
;; feel safe.
;;
;; Update: embedding the image in the text brings problems in indexing
;; the text, the best alternative is to embed the image data in a
;; separate supplement file “current-file-name.iimg”. If you want to
;; insert a image and store the data in the supplement file, answer yes
;; when asked when inserting the image.
;;
;;;; To enable:
;;
;;     M-x iimg-enable RET
;;
;;;; To insert an image:
;;
;; Drag and drop the image or use `iimg-insert'. Iimg will prompt for
;; a caption/name for the image. If you don’t enter anything, iimg
;; generates a unique string as the fallback.
;;
;; When you insert an image, the image appears at point is just a
;; link, the actual base64 data is appended at the end of the file (or
;; in the supplement file). I separate link and data because that way
;; readers incapable of rendering inline images can still view the
;; rest of the document without problems.
;;
;;;; To resize an image:
;;
;; Type s on an image or use `iimg-resize'. In the minibuffer, type in
;; the specification in the format of “SIDE UNIT AMOUNT”.
;;
;; SIDE can be width or height.
;; UNIT can be char or pixel.
;; AMOUNT can be a float or a integer.
;;
;; For example, “width char 40” means 40 characters wide. If AMOUNT is
;; a floating point number like 0.5, it is interpreted as a percentage
;; to the width/height of the window and UNIT is ignored.
;;
;; The default size is (width char 70).
;;
;;;; To toggle thumbnail display:
;;
;; Type t on an image or use `iimg-toggle-thumbnail'.
;;
;;;; To delete an image or image data
;;
;; To protect the image data, iimg marks them read-only, to delete
;; the data, press D on the data.
;;
;;;; To render an image across multiple lines:
;;
;; Type m on an image or use `iimg-toggle-multi-line'.
;;
;; When an image is displayed across multiple lines, scrolling is much
;; smoother. However, this doesn't work well when image size is set to
;; n percent of the window width/height: if you change the window
;; width/height, the number of lines needed for the image changes, but
;; iimg doesn't update its "image lines" automatically.
;;
;; Another way to get smooth scrolling over images is to use
;; iscroll.el. In that case you don't need to make the image
;; multi-line.

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
;;
;; Why text property instead of overlay: text property seems to be
;; faster (when scrolling, etc).

;;; Code:
;;
;; For `with-buffer-modified-unmodified'.
(require 'bookmark)

;;; Variables

(defvar-local iimg--data-alist nil
  "An alist of (NAME . IMAGE-DATA).
NAME (string) is the name of the image.
IMAGE-DATA is the image binary data.")

(defvar iimg-multi-line t
  "Render image in multiple lines.")

(defvar iimg-prune-slices-p t
  "If non-nil, iims prunes image slices when save.")

(defvar iimg--data-regexp (rx (seq "({iimg-data "
                                   (group (+? anything))
                                   "})"))
  "Regular expression for inline image data.
The first group is the plist containing data.")

(defvar iimg--link-regexp
  (rx (seq "({iimg-link " (group (+? anything)) "})"
           (group (* "\n---"))))
  "Regular expression for inline image link.
The first group is the plist containing data. The second group
contains the slices.")

(defsubst iimg--format-data (plist)
  "Return formatted iimg data.
PLIST is the plist part of the link, should be a plist."
  (format "({iimg-data %s})" (prin1-to-string plist)))

(defun iimg--format-link (plist)
  "Return formatted iimg link.
PLIST is the plist part of the link, should be a plist.
The image must already be in `iimg--data-alist'."
  (let* ((img (iimg--image-from-props plist))
         (multi-line (plist-get plist :multi-line))
         (row-count (ceiling (/ (cdr (image-size img t))
                                (frame-char-height)))))
    (format "({iimg-link %s})%s"
            (prin1-to-string plist)
            (if multi-line
                (with-temp-buffer
                  (dotimes (_ (1- row-count))
                    (insert "\n---"))
                  (buffer-string))
              ""))))

(defvar iimg--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "t" #'iimg-toggle-thumbnail)
    (define-key map "s" #'iimg-resize)
    (define-key map "d" #'iimg-delete-image-at-point)
    (define-key map "m" #'iimg-toggle-multi-line)
    map)
  "Keymap used on images.")

(defvar iimg--data-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "D" #'iimg-delete-data-at-point)
    map)
  "Keymap used on image data.")

;;; Loading and rendering


(defun iimg--image-from-props (props)
  "Given plist PROPS, return an image spec."
  (let* ((name (plist-get props :name))
         (thumbnail (plist-get props :thumbnail))
         (size (plist-get props :size))
         (size-spec (if thumbnail
                        ;; TODO This thumbnail size should work in most
                        ;; cases, but can be improved.
                        (iimg--calculate-size '(width char 30))
                      (iimg--calculate-size (or size '(width char 1.0))))))
    ;; Below `iimg--data-of' calls `iimg--load-image' which does
    ;; regexp search. I added `save-match-data' in `iimg--load-image'.
    (or (apply #'create-image (iimg--data-of name) nil t size-spec)
        (error "failed to create image"))))

(defun iimg--fontify-1 (beg end display)
  "Add overlay to text between BEG and END with DISPLAY property."
  (put-text-property beg end 'display display)
  (put-text-property beg end 'keymap iimg--link-keymap)
  (put-text-property beg end 'iimg t)
  (put-text-property beg end 'rear-nonstick '(display keymap iimg)))

(defun iimg--fontify (beg end)
  "Fontify embedded image links between BEG and END."
  (dolist (ov (overlays-in beg end))
    (if (overlay-get ov 'iimg)
        (delete-overlay ov)))
  ;; Fontify link.
  (goto-char beg)
  (while (and (re-search-forward iimg--link-regexp nil t)
              (< (match-beginning 0) end))
    ;; PROPS includes :name, :thumbnail, :size
    (let* ((props (read (match-string-no-properties 1)))
           (image (iimg--image-from-props props))
           (multi-line (plist-get props :multi-line))
           (name (plist-get props :name))
           (inhibit-read-only t))
      (cond
       ((not (display-graphic-p))
        ;; In terminal.
        (put-text-property
         (match-beginning 0) (match-end 0)
         'display (format "[iimg link of %s]" name)))
       ((not multi-line)
        ;; Render the image on a single line.
        (iimg--fontify-1 (match-beginning 0) (match-end 0) image))
       (t
        ;; Render the image across multiple lines. We assume the
        ;; number of placeholder lines in the buffer is correct.
        (save-excursion
          (let* ((slice-height (frame-char-height))
                 (image-width (car (image-size image t)))
                 (x 0) (y 0))
            (goto-char (match-beginning 0))
            (while (< (point) (match-end 0))
              (let ((beg (line-beginning-position))
                    (end (line-end-position)))
                (iimg--fontify-1
                 beg end (list (list 'slice x y image-width slice-height)
                               image))
                (put-text-property end (1+ end) 'line-height t)
                (setq y (+ y slice-height)))
              (forward-line))))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'read-only t)
      ;; This allows inserting after the image.
      (put-text-property (match-beginning 0) (match-end 0)
                         'rear-nonsticky
                         '(read-only display iimg keymap))))
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
Look for iimg-data’s and store them into an alist, then return it."
  ;; This could be called from within `iimg--fontify', and we
  ;; don’t want to mess up its match data.
  (save-match-data
    (save-excursion
      (goto-char beg)
      (let (alist)
        (while (re-search-forward iimg--data-regexp end t)
          (let* ((beg (match-beginning 1))
                 (end (match-end 1))
                 (props (read (buffer-substring-no-properties beg end)))
                 (name (plist-get props :name))
                 (base64-string (plist-get props :data))
                 (image-data (base64-decode-string base64-string)))
            (push (cons name image-data) alist)
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
                                   '(read-only display))
                (put-text-property beg end 'keymap iimg--data-keymap)
                (put-text-property beg end 'iimg t)))))
        alist))))

(defun iimg--supplement-file (&optional create)
  "Return the supplement-file if it exists, nil if not.
If CREATE non-nil, create the file when it doesn’t exist."
  (let ((supplement-file (concat (file-name-nondirectory
                                  (buffer-file-name))
                                 ".iimg")))
    (if (and create (not (file-exists-p supplement-file)))
        (with-temp-buffer
          (write-file supplement-file)))
    (if (file-exists-p supplement-file)
        supplement-file
      nil)))

(defun iimg--data-of (name)
  "Get the image data of NAME (string)."
  (when (not iimg--data-alist)
    (setq iimg--data-alist
          (append (iimg--load-image-data (point-min) (point-max))
                  (if-let ((file (iimg--supplement-file)))
                      (with-temp-buffer
                        (insert-file-contents file nil nil nil t)
                        (iimg--load-image-data
                         (point-min) (point-max)))))))
  (alist-get name iimg--data-alist nil nil #'equal))

(defun iimg--replenish-slices ()
  "Re-slice images.
We don't save the slices under a link, so we need to add them back."
  (save-excursion
    (with-buffer-modified-unmodified
     (goto-char (point-min))
     (while (re-search-forward iimg--link-regexp nil t)
       (when-let* ((props (read (buffer-substring-no-properties
                                 (match-beginning 1) (match-end 1))))
                   (multi-line (plist-get props :multi-line))
                   (inhibit-read-only t))
         (replace-match (iimg--format-link props)))))))

(defun iimg--prune-slices ()
  "Remove slices under a link before saving to a file."
  (save-excursion
    (let ((this-buffer (current-buffer))
          (this-file (buffer-file-name))
          (inhibit-read-only t))
      (with-temp-buffer
        (insert-buffer-substring this-buffer)
        (goto-char (point-min))
        (while (re-search-forward iimg--link-regexp nil t)
          (when-let ((beg (match-beginning 2))
                     (end (match-end 2)))
            (delete-region beg end)))
        (write-region (point-min) (point-max) this-file))
      (clear-visited-file-modtime)
      (set-buffer-modified-p nil)
      t)))

;;; Inserting and modifying

(defun iimg-insert (file name separate-file)
  "Insert FILE at point as an inline image.
NAME is the name of the image, THUMBNAIL determines whether to
display the image as a thumbnail, SIZE determines the size of the
image. See Commentary for the format of NAME, THUMBNAIL, and SIZE.

If SEPARATE-FILE non-nil, insert image data in a separate file
that appends .iimg extension to the current buffer’s filename."
  (interactive
   (list (expand-file-name (read-file-name "Image: "))
         (let ((name (read-string "Caption/name for the image: ")))
           (if (equal name "")
               (format-time-string "%s")
             name))
         (y-or-n-p "Insert in separate file? ")))
  (let* ((data (with-temp-buffer
                 (insert-file-contents-literally file nil nil nil t)
                 (base64-encode-region (point-min) (point-max))
                 ;; TODO Check for max image file size?
                 (buffer-string)))
         (data-string (iimg--format-data (list :name name :data data)))
         iimg-alist
         (insert-fn
          (lambda ()
            ;; Insert data.
            (save-excursion
              (goto-char (point-max))
              (when (text-property-any
                     (max (point-min) (1- (point))) (point) 'read-only t)
                (goto-char
                 (previous-single-char-property-change
                  (point) 'read-only)))
              (let ((beg (point)))
                (insert "\n" data-string "\n")
                (setq iimg-alist (iimg--load-image-data beg (point))))))))
    (if separate-file
        (let ((supp-file (iimg--supplement-file t)))
          (with-temp-buffer
            (insert-file-contents supp-file nil nil nil t)
            (funcall insert-fn)
            (write-file supp-file)))
      (funcall insert-fn))
    (setq iimg--data-alist (append iimg-alist iimg--data-alist))
    ;; Insert link. We insert link after loading image data.
    (insert (iimg--format-link
             (list :name name :size '(width pixel 0.6)
                   :ext (file-name-extension file))))))

(defun iimg--search-link-at-point (&optional datap)
  "Search for iimg link at point.
If found, set match data accordingly and return t, if not, return nil.

If DATAP non-nil, search for image data instead."
  (catch 'found
    (save-excursion
      (let ((pos (point)))
        (beginning-of-line)
        ;; First search in current line.
        (while (and (<= (point) pos)
                    (re-search-forward
                     (if datap iimg--data-regexp iimg--link-regexp)
                     nil t))
          (if (<= (match-beginning 0) pos (match-end 0))
              (throw 'found t)))
        ;; Next search by search backward.
        (goto-char pos)
        (if (and (search-backward
                  (if datap "({iimg-data" "({iimg-link")
                  nil t)
                 (re-search-forward
                  (if datap iimg--data-regexp iimg--link-regexp)
                  nil t)
                 (<= (match-beginning 0) pos (match-end 0)))
            (throw 'found t))))))

(defun iimg--link-at-point ()
  "Return the data (plist) of the iimg link at point.
Return nil if not found."
  (if (iimg--search-link-at-point)
      (read (match-string 1))
    nil))

(defun iimg--set-link-at-point-refresh (props)
  "Set iimg link at point to PROPS, if there is any link.
Also refresh the image at point."
  (when (iimg--search-link-at-point)
    (save-excursion
      (let ((beg (match-beginning 0))
            (inhibit-read-only t))
        (goto-char beg)
        (delete-region beg (match-end 0))
        (insert (iimg--format-link props))
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

(defun iimg-toggle-multi-line ()
  "Toggle multi-line display for the image at point."
  (interactive)
  (if-let ((img-props (iimg--link-at-point)))
      (progn (setq img-props
                   (plist-put img-props :multi-line
                              (not (plist-get img-props :multi-line))))
             (iimg--set-link-at-point-refresh img-props))
    (user-error "There is no image at point")))

(defun iimg-delete-image-at-point ()
  "Delete the image at point."
  (interactive)
  (if (iimg--search-link-at-point)
      (let ((inhibit-read-only t))
        (delete-region (match-beginning 0) (match-end 0)))
    (user-error "There is no image at point")))

(defun iimg-delete-data-at-point ()
  "Delete the image data at point."
  (interactive)
  (if (iimg--search-link-at-point t)
      (let ((inhibit-read-only t))
        (delete-region (match-beginning 0) (match-end 0)))
    (user-error "There is no image data at point")))

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

;;; Entry

(defun iimg-enable ()
  "Enable iimg."
  (jit-lock-register #'iimg--fontify)
  (setq-local dnd-protocol-alist
              (cons '("^file:" . iimg-dnd-open-file)
                    dnd-protocol-alist))
  (when iimg-prune-slices-p
    (add-hook 'write-file-functions #'iimg--prune-slices 90 t))
  (iimg--replenish-slices))

;;; Drag and drop

(defun iimg-dnd-open-file (uri _action)
  "Drag-and-drop handler for iimg. URI is the file path."
  (let ((file (dnd-get-local-file-name uri t)))
    (if (and file (file-readable-p file))
        (iimg-insert
         file (let ((name (read-string "Caption/name for the image: ")))
                (if (equal name "")
                    (format-time-string "%s")
                  name))
         (y-or-n-p "Insert in separate file? ")))))

(provide 'iimg)

;;; iimg.el ends here
