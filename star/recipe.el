;; -*- lexical-binding: t -*-

(setq cowboy-recipe-alist
      '((matlab . (:http "https://git.code.sf.net/p/matlab-emacs/src"))
        (nerd-font . (:repo "twlz0ne/nerd-fonts.el"))
        (julia-emacs . (:repo "JuliaEditorSupport/julia-emacs"))
        (separedit . (:repo "twlz0ne/separedit.el"
                            :dependency (edit-indirect dash)))
        (package-demo . (:repo "vermiculus/package-demo"))
        (sly-el-indent . (:repo "cireu/sly-el-indent"))
        (cowboy-test . (:repo "casouri/cowboy-test"))
        (binder . (:repo "rnkn/binder"))
        (wgrep . (:repo "mhayashi1120/Emacs-wgrep"))
        (multi-translate . (:repo "twlz0ne/multi-translate.el"
                                  :dependency (bing-dict
                                               google-translate
                                               youdao-dictionary
                                               sdcv)))
        (phscroll . (:repo "misohena/phscroll"))
        (svg-tag-mode . (:repo "rougier/svg-tag-mode"))
        (scroll-on-jump . (:http "https://gitlab.com/ideasman42/emacs-scroll-on-jump.git"))
        (emacs-svg-icon . (:repo "rougier/emacs-svg-icon"))
        ;; Full clones.
        (expand-region . (:repo "casouri/expand-region"
                                :option (:full-clone t)))
        (deft . (:repo "casouri/deft" :option (:full-clone t)))
        (isolate . (:repo "casouri/isolate" :option (:full-clone t)))
        (nyan-lite . (:repo "casouri/nyan-lite" :option (:full-clone t)))
        (ftable . (:repo "casouri/ftable" :option (:full-clone t)))
        (ghelp . (:repo "casouri/ghelp" :option (:full-clone t)))
        (iscroll . (:repo "casouri/iscroll" :option (:full-clone t)))
        (valign . (:repo "casouri/valign" :option (:full-clone t)))
        (zeft . (:repo "casouri/zeft" :option (:full-clone t)))))
