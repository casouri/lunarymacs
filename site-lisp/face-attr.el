;;; face-attr.el --- Face attribute functions for text property      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; face functions only works with face symbols, but text property
;; accepts much more than just symbols. This package provides functions
;; that properly handles all possible values that can be in a 'face
;; text property.
;;
;; Note:
;;
;; Before you use this package, ask yourself if you really have to:
;; normally overlays are simpler alternatives.

;;; Code:
;;

(defun valign--face-attribute (face attribute &optional frame inherit)
  "Return ATTRIBUTE of FACE.
FACE can be anything a 'face text property accepts."
  ;;    1. Simply nil.
  (cond ((null face) nil)
        ;; 2. A face symbol.
        ((symbolp face) (face-attribute face attribute frame inherit))
        ((proper-list-p face)
         ;;    3. (:filter FILTER SPEC)
         (cond ((eq (car face) :filter)
                `(:filter ,(cadr face)
                          ,(valign--face-attribute
                            (caddr face) attribute frame inherit)))
               ;; 4. A plist face-spec.
               ((keywordp (car face))
                (or (plist-get face attribute)
                    (and inherit (face-attribute inherit attribute))))
               ;; 5. A list of faces.  (We don’t check if it really is
               ;; a list of faces.)
               (t (car (cl-loop
                        for f in face
                        collect
                        (valign--face-attribute f frame inherit))))))
        ;; 6. (foreground-color . COLOR-NAME)
        ((and (consp face) (eq (car face) 'foreground-color))
         (plist-get (list :foreground (cdr face)) attribute))
        ;; 7. (background-color . COLOR-NAME)
        ((and (consp face) (eq (car face) 'background-color))
         (plist-get (list :background (cdr face)) attribute))
        (t (error "Valign encountered a invalid face: %s" face))))

(defun valign--set-face-attribute (face attribute value)
  "Return FACE with ATTRIBUTE set to VALUE.
FACE can be anything a 'face text property accepts."
  ;;    1. Simply nil.
  (cond ((null face) (list attribute value))
        ;; 2. A face symbol.
        ((symbolp face) (list attribute value :inherit face))
        ((proper-list-p face)
         ;;    3. (:filter FILTER SPEC)
         (cond ((eq (car face) :filter)
                `(:filter ,(cadr face)
                          ,(valign--set-face-attribute
                            (caddr face) attribute value)))
               ;; 4. A plist face-spec.
               ((keywordp (car face))
                (if (eq (plist-get face attribute) value)
                    face
                  (plist-put (copy-tree face) attribute value)))
               ;; 5. A list of faces.  (We don’t check if it really is
               ;; a list of faces.)
               (t (if (cl-find `(,attribute ,value) face
                               :test #'equal)
                      face
                    (cons `(,attribute ,value) face)))))
        ;; 6. (foreground-color . COLOR-NAME)
        ((and (consp face) (eq (car face) 'foreground-color))
         (list attribute value :foreground (cdr face)))
        ;; 7. (background-color . COLOR-NAME)
        ((and (consp face) (eq (car face) 'background-color))
         (list attribute value :background (cdr face)))
        (t (error "Valign encountered a invalid face: %s" face))))

(when nil
  (ert-deftest valign--test-set-face-attribute ()
    (let* ((face0 nil)
           (face1 'face-sym)
           (face2 '(:filter <filter> face-sym))
           (face3 '(:foreground "blue"))
           (face4 '(face-sym1 face-sym2))
           (face5 '(foreground-color . "blue"))
           (face6 '(background-color . "blue"))
           ;; Apply once.
           (face01 (valign--set-face-attribute face0 :strike-through t))
           (face11 (valign--set-face-attribute face1 :strike-through t))
           (face21 (valign--set-face-attribute face2 :strike-through t))
           (face31 (valign--set-face-attribute face3 :strike-through t))
           (face41 (valign--set-face-attribute face4 :strike-through t))
           (face51 (valign--set-face-attribute face5 :strike-through t))
           (face61 (valign--set-face-attribute face6 :strike-through t))
           ;; Apply twice
           (face02 (valign--set-face-attribute face01 :strike-through t))
           (face12 (valign--set-face-attribute face11 :strike-through t))
           (face22 (valign--set-face-attribute face21 :strike-through t))
           (face32 (valign--set-face-attribute face31 :strike-through t))
           (face42 (valign--set-face-attribute face41 :strike-through t))
           (face52 (valign--set-face-attribute face51 :strike-through t))
           (face62 (valign--set-face-attribute face61 :strike-through t))
           ;; Correct value
           (face0c '(:strike-through t))
           (face1c '(:strike-through t :inherit face-sym))
           (face2c '(:filter <filter>
                             (:strike-through t :inherit face-sym)))
           (face3c '((:foreground "blue" :strike-through t)
                     (:strike-through t :foreground "blue")))
           (face4c '((:strike-through t) face-sym1 face-sym2))
           (face5c '((:foreground "blue" :strike-through t)
                     (:strike-through t :foreground "blue")))
           (face6c '((:background "blue" :strike-through t)
                     (:strike-through t :background "blue"))))
      (should (equal face01 face0c))
      (should (equal face11 face1c))
      (should (equal face21 face2c))
      (should (member face31 face3c))
      (should (equal face41 face4c))
      (should (member face51 face5c))
      (should (member face61 face6c))

      (should (equal face02 face0c))
      (should (equal face12 face1c))
      (should (equal face22 face2c))
      (should (member face32 face3c))
      (should (equal face42 face4c))
      (should (member face52 face5c))
      (should (member face62 face6c)))))

(provide 'face-attr)

;;; face-attr.el ends here
