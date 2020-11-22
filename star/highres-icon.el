;; -*- lexical-binding: t; -*-

;;; Image.el

(setq image-resolution-scaling-factor 2)
(push (expand-file-name "etc/images/" user-emacs-directory)
      image-load-path)

(defun find-image (specs)
  "Find an image, choosing one of a list of image specifications.

SPECS is a list of image specifications.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The specification whose TYPE
is supported, and FILE exists, is used to construct the image
specification to be returned.  Return nil if no specification is
satisfied.

The image is looked for in `image-load-path'.

Image files should not be larger than specified by `max-image-size'."
  (let (image)
    (while (and specs (null image))
      (let* ((spec (car specs))
	     (type (plist-get spec :type))
	     (data (plist-get spec :data))
	     (file (plist-get spec :file))
             (high-res (when (> image-resolution-scaling-factor 1)
                         (format "%s%s@%dx.%s"
                                 (or (file-name-directory file) "")
                                 (file-name-base file)
                                 image-resolution-scaling-factor
                                 (or (file-name-extension file) ""))))
	     found)
	(when (image-type-available-p type)
	  (cond ((stringp file)
                 (let ((high-res-found
                        (and high-res (image-search-load-path high-res)))
                       (found (image-search-load-path file)))
                   (when (or high-res-found found)
                     (let ((spec (copy-sequence spec)))
                       (plist-put spec :file (or high-res-found found))
                       (when high-res-found
                         (plist-put
                          spec :scale
                          (/ 1.0 image-resolution-scaling-factor)))
                       (setq image (cons 'image spec))))))
		((not (null data))
		 (setq image (cons 'image spec)))))
	(setq specs (cdr specs))))
    image))

;;; Tab-line

(with-eval-after-load 'tab-line
  (setq tab-line-tab-name-function
        (lambda (buffer &optional _)
          (format " %s " (buffer-name buffer))))

  (setq tab-line-new-button
        (propertize " + "
                    'display `(image :type xpm
                                     :file ,(image-search-load-path
                                             "tabs/new@2x.xpm")
                                     :margin (2 . 0)
                                     :ascent center
                                     :scale 0.5)
                    'keymap tab-line-add-map
                    'mouse-face 'tab-line-highlight
                    'help-echo "Click to add tab"))

  (setq tab-line-close-button
        (propertize " x"
                    'display `(image :type xpm
                                     :file ,(image-search-load-path
                                             "tabs/close@2x.xpm")
                                     :margin (2 . 0)
                                     :ascent center
                                     :scale 0.5)
                    'keymap tab-line-tab-close-map
                    'mouse-face 'tab-line-close-highlight
                    'help-echo "Click to close tab"))

  (setq tab-line-left-button
        (propertize " <"
                    'display `(image :type xpm
                                     :file ,(image-search-load-path
                                             "tabs/left-arrow@2x.xpm")
                                     :margin (2 . 0)
                                     :ascent center
                                     :scale 0.5)
                    'keymap tab-line-left-map
                    'mouse-face 'tab-line-highlight
                    'help-echo "Click to scroll left"))

  (setq tab-line-right-button
        (propertize "> "
                    'display `(image :type xpm
                                     :file ,(image-search-load-path
                                             "tabs/right-arrow@2x.xpm")
                                     :margin (2 . 0)
                                     :ascent center
                                     :scale 0.5)
                    'keymap tab-line-right-map
                    'mouse-face 'tab-line-highlight
                    'help-echo "Click to scroll right")))

;;; Widget

(setq widget-image-directory "~/ed/etc/images/custom/")
