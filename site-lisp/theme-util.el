;;; theme-util.el --- Theme tools      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.cam>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'seq)
(require 'cl-lib)

;;; Define theme

(defun theme-util-deffaces (&rest face-list)
  "FACE-LIST is a list of (FACE DOC)."
  (dolist (face face-list)
    (face-spec-set face '((t . (:inherit default)))
                   'face-defface-spec)))

(defun theme-util-set-faces (name spec)
  "Define theme of NAME with SPEC on DISPLAY.
SPEC is as in ‘theme-util-make-face-spec’."
  (declare (indent 1))
  (apply #'custom-theme-set-faces
         name
         (mapcar #'theme-util-make-face-spec
                 spec)))

(defun theme-util-make-face-spec (spec)
  "Convert SPEC into actual spec used in ‘custom-theme-set-faces’.

SPEC is a list

    \(FACE (INHERIT FOREGROUND BACKGROUND UNDERLINE WEIGHT SLANT)
          REST-ATTR DISPLAY)

REST-ATTR is a plist (:key :value ...). DISPLAY is the same as in
`defface'.

For example,
\(default nil \"white\" \"black\" nil 'bold nil (:height 10))."
  (let* ((face (nth 0 spec))
         (attr (nth 1 spec))
         (rest-attr (nth 2 spec))
         (display (nth 3 spec))
         (inherit (nth 0 attr))
         (fg (nth 1 attr))
         (bg (nth 2 attr))
         (underline (nth 3 attr))
         (weight (nth 4 attr))
         (slant (nth 5 attr)))
    `(,face ((,(or display t)
              . ,(remove
                  nil
                  (append (if inherit (list :inherit inherit))
                          (if fg (list :foreground fg))
                          (if bg (list :background bg))
                          (if underline (list :underline underline))
                          (if weight (list :weight weight))
                          (if slant (list :slant slant))
                          rest-attr)))))))

;;; Inspect theme

(when (featurep 'hierarchy)
  (defun theme-util-top-level-face-to-kill-ring (regexp)
    (interactive "sRegexp: ")
    (kill-new
     (string-join
      (mapcar
       (lambda (face)
         (format "(%s :inherit 'default)\n" face))
       (sort (seq-filter (lambda (face)
                           (and (string-match regexp (face-name face))
                                (not (face-attribute face :inherit nil 'default))))
                         (face-list))
             (lambda (f1 f2)
               (string-lessp (face-name f1) (face-name f2))))))))

  (defun theme-util-show-face-tree (&optional regexp)
    (interactive)
    (switch-to-buffer
     (let ((tree (hierarchy-new))
           (parent-fn
            (lambda (face)
              (let ((parent-face (if (eq face 'root-face)
                                     nil ;; the root has no parent
                                   (or (face-attribute face :inherit nil 'default)
                                       'root-face ))))
                (cond ((facep parent-face) parent-face)
                      ((null parent-face) nil)
                      (t 'root-face)))))
           (face-list (seq-filter (lambda (face)
                                    (if (not regexp)
                                        t
                                      (string-match regexp (face-name face))))
                                  (face-list))))
       (hierarchy-add-trees tree face-list parent-fn)
       (hierarchy-tree-display tree (lambda (face _) (insert (format "%s" face)))))))
  )
;;; Color util

(defvar theme-util-color-distance-fn
  (lambda (c1 c2)
    (let ((r1 (nth 0 c1))
          (g1 (nth 1 c1))
          (b1 (nth 2 c1))
          (r2 (nth 0 c2))
          (g2 (nth 1 c2))
          (b2 (nth 2 c2)))
      (+ (expt (- r1 r2) 2)
         (expt (- g1 g2) 2)
         (expt (- b1 b2) 2))))
  "Function that returns the distance between color1 and color2.
Should take two colors and return a number. There is no
specification on the range of the returned number as long as
greater number implies greater distance. Each color is like (R G
B) where R, G, B are number.

More on https://en.wikipedia.org/wiki/Color_difference")

(defun theme-util-color-distance (color1 color2)
  "Return the distance between COLOR1 and COLOR2.
COLOR’s are in the form of ”#RRGGBB”."
  (funcall theme-util-color-distance-fn
           (theme-util-color-str-to-list color1)
           (theme-util-color-str-to-list color2)))

(defun theme-util-color-str-to-list (color)
  "Convert COLOR in ”#RRGGBB” format to (R G B)."
  (list (string-to-number (substring color 1 3) 16)
        (string-to-number (substring color 3 5) 16)
        (string-to-number (substring color 5 7) 16)))

(defun theme-util-color-list-to-str (color)
  "Convert COLOR in (R G B) format to ”#RRGGBB”."
  (format "#%.2x%.2x%.2x" (nth 0 color) (nth 1 color) (nth 2 color)))

(defun theme-util--closest-color-in-list (color color-list)
  "Return the color closest to COLOR from COLOR-LIST."
  (let (closest-color
        (min-distance 1.0e+INF))
    (dolist (a-color color-list)
      (let ((new-dist (theme-util-color-distance color a-color)))
        (when (< new-dist min-distance)
          (setq min-distance new-dist
                closest-color a-color))))
    closest-color))

(defun theme-util-closest-8-bit-color (color)
  "Return the closest 8 bit color to COLOR."
  (theme-util-closest-color-in-list color theme-util--8-bit-color-list))

(defun theme-util-closest-4-bit-color (color)
  "Return the closest 4 bit color to COLOR."
  (theme-util-closest-color-in-list color theme-util--4-bit-color-list))

(defun theme-util-color-overlay (color-base color-above alpha)
  "Return a color made of COLOR-ABOVE with alpha ALPHA placed above COLOR-BASE.
Both COLOR’S are like ”#RRGGBB”, ALPHA is a float between 0 and 1."
  (theme-util-color-list-to-str
   (cl-labels ((comp (base above alpha)
                 (+ (* base (- 1 alpha)) (* above alpha)))
               (bound (color) (cond ((> color 255) 255)
                                    ((< color 0) 0)
                                    (t color))))
     (let* ((color-base (theme-util-color-str-to-list color-base))
            (color-above (theme-util-color-str-to-list color-above)))
       (cl-loop for base in color-base
                for above in color-above
                collect (bound (comp base above alpha)))))))

(defun theme-util-adjust-brightness (color brightness)
  "Adjust the BRIGHTNESS of COLOR.
Basically minmax(0, R/G/B * brightness, 255).
COLOR is like ”#RRGGBB”."
  (theme-util-color-list-to-str
   (mapcar (lambda (channel) (min 255 (* brightness channel)))
           (theme-util-color-str-to-list color))))

(defun theme-util-darken (color degree)
  "Darken COLOR by DEGREE (float between 0 and 1)."
  (theme-util-color-overlay color "#000000" degree))

(defun theme-util-brighten (color degree)
  "Brighten COLOR by DEGREE (float between 0 and 1)."
  (theme-util-color-overlay color "#ffffff" degree))

(defvar theme-util--8-bit-color-list
  '("#000000"
    "#800000"
    "#008000"
    "#808000"
    "#000080"
    "#800080"
    "#008080"
    "#c0c0c0"
    "#808080"
    "#ff0000"
    "#00ff00"
    "#ffff00"
    "#0000ff"
    "#ff00ff"
    "#00ffff"
    "#ffffff"
    "#000000"
    "#00005f"
    "#000087"
    "#0000af"
    "#0000d7"
    "#0000ff"
    "#005f00"
    "#005f5f"
    "#005f87"
    "#005faf"
    "#005fd7"
    "#005fff"
    "#008700"
    "#00875f"
    "#008787"
    "#0087af"
    "#0087d7"
    "#0087ff"
    "#00af00"
    "#00af5f"
    "#00af87"
    "#00afaf"
    "#00afd7"
    "#00afff"
    "#00d700"
    "#00d75f"
    "#00d787"
    "#00d7af"
    "#00d7d7"
    "#00d7ff"
    "#00ff00"
    "#00ff5f"
    "#00ff87"
    "#00ffaf"
    "#00ffd7"
    "#00ffff"
    "#5f0000"
    "#5f005f"
    "#5f0087"
    "#5f00af"
    "#5f00d7"
    "#5f00ff"
    "#5f5f00"
    "#5f5f5f"
    "#5f5f87"
    "#5f5faf"
    "#5f5fd7"
    "#5f5fff"
    "#5f8700"
    "#5f875f"
    "#5f8787"
    "#5f87af"
    "#5f87d7"
    "#5f87ff"
    "#5faf00"
    "#5faf5f"
    "#5faf87"
    "#5fafaf"
    "#5fafd7"
    "#5fafff"
    "#5fd700"
    "#5fd75f"
    "#5fd787"
    "#5fd7af"
    "#5fd7d7"
    "#5fd7ff"
    "#5fff00"
    "#5fff5f"
    "#5fff87"
    "#5fffaf"
    "#5fffd7"
    "#5fffff"
    "#870000"
    "#87005f"
    "#870087"
    "#8700af"
    "#8700d7"
    "#8700ff"
    "#875f00"
    "#875f5f"
    "#875f87"
    "#875faf"
    "#875fd7"
    "#875fff"
    "#878700"
    "#87875f"
    "#878787"
    "#8787af"
    "#8787d7"
    "#8787ff"
    "#87af00"
    "#87af5f"
    "#87af87"
    "#87afaf"
    "#87afd7"
    "#87afff"
    "#87d700"
    "#87d75f"
    "#87d787"
    "#87d7af"
    "#87d7d7"
    "#87d7ff"
    "#87ff00"
    "#87ff5f"
    "#87ff87"
    "#87ffaf"
    "#87ffd7"
    "#87ffff"
    "#af0000"
    "#af005f"
    "#af0087"
    "#af00af"
    "#af00d7"
    "#af00ff"
    "#af5f00"
    "#af5f5f"
    "#af5f87"
    "#af5faf"
    "#af5fd7"
    "#af5fff"
    "#af8700"
    "#af875f"
    "#af8787"
    "#af87af"
    "#af87d7"
    "#af87ff"
    "#afaf00"
    "#afaf5f"
    "#afaf87"
    "#afafaf"
    "#afafd7"
    "#afafff"
    "#afd700"
    "#afd75f"
    "#afd787"
    "#afd7af"
    "#afd7d7"
    "#afd7ff"
    "#afff00"
    "#afff5f"
    "#afff87"
    "#afffaf"
    "#afffd7"
    "#afffff"
    "#d70000"
    "#d7005f"
    "#d70087"
    "#d700af"
    "#d700d7"
    "#d700ff"
    "#d75f00"
    "#d75f5f"
    "#d75f87"
    "#d75faf"
    "#d75fd7"
    "#d75fff"
    "#d78700"
    "#d7875f"
    "#d78787"
    "#d787af"
    "#d787d7"
    "#d787ff"
    "#d7af00"
    "#d7af5f"
    "#d7af87"
    "#d7afaf"
    "#d7afd7"
    "#d7afff"
    "#d7d700"
    "#d7d75f"
    "#d7d787"
    "#d7d7af"
    "#d7d7d7"
    "#d7d7ff"
    "#d7ff00"
    "#d7ff5f"
    "#d7ff87"
    "#d7ffaf"
    "#d7ffd7"
    "#d7ffff"
    "#ff0000"
    "#ff005f"
    "#ff0087"
    "#ff00af"
    "#ff00d7"
    "#ff00ff"
    "#ff5f00"
    "#ff5f5f"
    "#ff5f87"
    "#ff5faf"
    "#ff5fd7"
    "#ff5fff"
    "#ff8700"
    "#ff875f"
    "#ff8787"
    "#ff87af"
    "#ff87d7"
    "#ff87ff"
    "#ffaf00"
    "#ffaf5f"
    "#ffaf87"
    "#ffafaf"
    "#ffafd7"
    "#ffafff"
    "#ffd700"
    "#ffd75f"
    "#ffd787"
    "#ffd7af"
    "#ffd7d7"
    "#ffd7ff"
    "#ffff00"
    "#ffff5f"
    "#ffff87"
    "#ffffaf"
    "#ffffd7"
    "#ffffff"
    "#080808"
    "#121212"
    "#1c1c1c"
    "#262626"
    "#303030"
    "#3a3a3a"
    "#444444"
    "#4e4e4e"
    "#585858"
    "#606060"
    "#666666"
    "#767676"
    "#808080"
    "#8a8a8a"
    "#949494"
    "#9e9e9e"
    "#a8a8a8"
    "#b2b2b2"
    "#bcbcbc"
    "#c6c6c6"
    "#d0d0d0"
    "#dadada"
    "#e4e4e4"
    "#eeeeee"))

(defvar theme-util--4-bit-color-list
  '("#000000"
    "#0000FF"
    "#00FF00"
    "#00FFFF"
    "#000080"
    "#008000"
    "#008080"
    "#800000"
    "#800080"
    "#808000"
    "#808080"
    "#C0C0C0"
    "#FF0000"
    "#FF00FF"
    "#FFFF00"
    "#FFFFFF"))

(provide 'theme-util)

;;; theme-util.el ends here
