;; -*- lexical-binding: t; -*-

(setq image-resolution-scaling-factor 2)

(push "~/ed/etc/images/" image-load-path)

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
                  'help-echo "Click to scroll right"))

(setq widget-image-directory "~/ed/etc/images/custom/")
