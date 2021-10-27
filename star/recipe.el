;; -*- lexical-binding: t -*-

(setq cowboy-recipe-alist
      '((matlab . (:http "https://git.code.sf.net/p/matlab-emacs/src"))
        ;; Full clones.
        (expand-region . (:repo "casouri/expand-region.el"
                                :option (:full-clone t)))
        (isolate . (:repo "casouri/isolate" :option (:full-clone t)))
        (ftable . (:repo "casouri/ftable" :option (:full-clone t)))
        (ghelp . (:repo "casouri/ghelp" :option (:full-clone t)))
        (iscroll . (:repo "casouri/iscroll" :option (:full-clone t)))
        (valign . (:repo "casouri/valign" :option (:full-clone t)))
        (zeft . (:repo "casouri/zeft" :option (:full-clone t)))
        (xeft . (:repo "casouri/xeft" :option (:full-clone t)))
        (vundo . (:repo "casouri/vundo" :option (:full-clone t)))))
