;;; luna-key.el --- General.el for Lunarymacs      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'pcase)

(defvar luna-key-preset-alist nil
  "Stores defined presets.")

(defvar luna-key-override-mode-map (make-sparse-keymap)
  "One map to rule them all.")

(defvar luna-key-override-map-alist
  `(luna-key-override-mode . ,luna-key-override-mode-map)
  "Alist put into `emulation-mode-map-alists' to override all other maps.")

(defvar luna-key-postponed-alist nil
  "An alist of (map . ((key def) (key def))).
When we define a key in a map that’s not defined yet, we push the
definition to this alist. When a new file is loaded, we check for
each map and see if we can now define the bindings for that map.")

(defun luna-key-define-postponed-binding (_)
  "Define postponed bindings in ‘luna-key-postponed-alist’."
  (dolist (map (mapcar #'car luna-key-postponed-alist))
    (when (boundp map)
      (pcase-dolist (`(,key ,def)
                     (alist-get map luna-key-postponed-alist))
        (define-key (symbol-value map) key def))
      (setf (alist-get map luna-key-postponed-alist) nil))))

(define-minor-mode luna-key-override-mode
  "Minor mode used to activate our override keymap."
  :global t
  :lighter ""
  :keymap 'luna-key-override-mode-map)

(defun luna-key-def-preset (preset &rest args)
  "Define PRESET as ARGS.

ARGS can be anything valid in `luna-def-key'.

If you define :leader as

    (luna-key-def-preset :leader
     :keymaps 'override
     :prefix \"C-SPC\")

Then

    (luna-def-key
     :leader
     KEY DEF)

is equivalent to

    (luna-def-key
     :keymaps 'override
     :prefix \"C-SPC\"
     KEY DEF)"
  (declare (indent 1))
  (setf (alist-get preset luna-key-preset-alist) args))

(defun luna-key-normalize (prefix key)
  "Normalize KEY and PREFIX with `kbd' and combine them.
However, if KEY is [remap ...] or [t], don’t prepend PREFIX to it."
  ;; Normalize KEY and PREFIX with `kbd'.
  (if (stringp key) (setq key (kbd key)))
  (if (and prefix (stringp prefix)) (setq prefix (kbd prefix)))
  ;; Result of ‘kbd’ can be either string or vector,
  ;; new we normalize KEY and PREFIX to vectors.
  (if (stringp key) (setq key (string-to-vector key)))
  (if (and prefix (stringp prefix))
      (setq prefix (string-to-vector prefix)))
  ;; When do we simply return KEY without PREFIX: either PREFIX is
  ;; nil, or KEY is has special meaning ([remap ...] or [t]).
  (if (or (not prefix) (and (vectorp key) (memq (aref key 0) '(t remap))))
      key
    (vconcat prefix key)))

(defun luna-key-define (key def map-list prefix)
  "Define KEY to DEF.
Define KEY in all the maps in MAP-LIST, using PREFIX as prefix. 
MAP-LIST and PREFIX can be nil."
  (let ((map-list (or map-list (list 'global-map)))
        (key (luna-key-normalize prefix key))
        ;; If DEF is (STRING . DEFN), we use STRING as it’s description.
        (desc (car-safe def)))
    (when desc
      (with-eval-after-load 'which-key
        (which-key-add-key-based-replacements
          (key-description key) desc)))
    (dolist (map map-list)
      (let ((map (if (eq map 'override) 'luna-key-override-mode-map map)))
        (if (boundp map)
            (define-key (symbol-value map) key def)
          (push (list key def)
                (alist-get map luna-key-postponed-alist))
          (add-hook 'after-load-functions
                    #'luna-key-define-postponed-binding))))))

(defun luna-def-key (&rest args)
  "Define key.

The :keymaps and :prefix command specifies the keymaps and prefix
key for KEY DEF pairs below them. It is also possible to specify
different maps for different keys. To define KEY1 in MAP1 and
KEY2 in MAP2:

  (luna-def-key
   :keymaps 'MAP1
   KEY1 DEF1
   :keymaps 'MAP2n
   KEY2 DEF2)

Unlike in `define-key', MAP is a symbol of a keymap, rather than
the keymap itself. MAP can also be nil, which is interpreted as
`global-map', or 'override, which is interpreted as a override
keymap defined by luna-key, or a list of these three forms. KEY
and DEF can be anything that `define-key' accepts.

You can also define presets with `luna-key-def-preset' and use it
here. They are basically macros that expand to commands.

Use :clear to reset all :keymap and :prefix settings in-effect.

ARGS.

\(fn [:keymaps MAPS] [:prefix PREFIX] [:clear] KEY DEF ...)"
  (luna-key-override-mode)
  (condition-case nil
      (let (arg map-list prefix)
        (while args
          (setq arg (pop args))
          (pcase arg
            (:keymaps
             (let ((map (pop args)))
               (cond ((symbolp map) (setq map-list (list map)))
                     ((proper-list-p map) (setq map-list map))
                     (t (error "Invalid argument for :keymaps command: %s"
                               map)))))
            (:prefix
             (setq prefix (pop args)))
            (:clear (setq prefix nil
                          map-list nil))
            ((pred keywordp)
             (when-let ((preset (alist-get arg luna-key-preset-alist)))
               (setq args (append preset args))))
            (_ (let ((key arg)
                     (def (pop args)))
                 (luna-key-define key def map-list prefix))))))
    (setting-constant (error "Not enough arguments"))))

(provide 'luna-key)

;;; luna-key.el ends here
