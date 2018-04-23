;;; -*- lexical-binding: t -*-

(use-package| web-mode
  :mode
  "\\.phtml\\'"
  "\\.tpl\\.php\\'"
  "\\.[agj]sp\\'"
  "\\.as[cp]x\\'"
  "\\.erb\\'"
  "\\.mustache\\'"
  "\\.djhtml\\'"
  "\\.html?\\'"
  :config (when (featurep 'flycheck )
            (flycheck-add-mode 'html-tidy 'web-mode)))
