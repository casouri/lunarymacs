;; -*- lexical-binding: t -*-

(setq cowboy-recipe-alist
      '((matlab . (:http "https://git.code.sf.net/p/matlab-emacs/src"))
        (breadcrumb . (:repo "joaotavora/breadcrumb"))
        (inspector . (:repo "mmontone/emacs-inspector"))
        ;; Full clones.
        (expand-region . (:repo "casouri/expand-region.el"
                                :option (:full-clone t)))
        (expreg . (:repo "casouri/expreg" :option (:full-clone t)))
        (eldoc-box . (:repo "casouri/eldoc-box"
                            :option (:full-clone t)))
        (eglot-booster . (:repo "jdtsmith/eglot-booster"))
        (isolate . (:repo "casouri/isolate" :option (:full-clone t)))
        (ftable . (:repo "casouri/ftable" :option (:full-clone t)))
        (ghelp . (:repo "casouri/ghelp" :option (:full-clone t)))
        (iscroll . (:repo "casouri/iscroll" :option (:full-clone t)))
        (valign . (:repo "casouri/valign" :option (:full-clone t)))
        (zeft . (:repo "casouri/zeft" :option (:full-clone t)))
        (xeft . (:repo "casouri/xeft" :option (:full-clone t)))
        (vundo . (:repo "casouri/vundo" :option (:full-clone t)))
        (undo-hl . (:repo "casouri/undo-hl" :option (:full-clone t)))
        (ultra-scroll . (:repo "jdtsmith/ultra-scroll"))
        (stimmung-themes . (:repo "motform/stimmung-themes" :option (:full-clone t)))
        (haskell-ts-mode . (:http "https://codeberg.org/pranshu/haskell-ts-mode.git"))
        (restclient . (:http "https://github.com/casouri/restclient.el.git"))))
