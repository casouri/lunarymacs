(setq cowboy-recipe-alist
      '((isolate . (:repo "casouri/isolate" :option (:full-clone t)))
        (nyan-lite . (:repo "casouri/nyan-lite" :option (:full-clone t)))
        (matlab . (:http "https://git.code.sf.net/p/matlab-emacs/src"))
        (nerd-font . (:repo "twlz0ne/nerd-fonts.el"))
        (julia-emacs . (:repo "JuliaEditorSupport/julia-emacs"))
        (separedit . (:repo "twlz0ne/separedit.el"
                            :dependency (edit-indirect dash)))
        (package-demo . (:repo "vermiculus/package-demo"))
        (ghelp . (:repo "casouri/ghelp" :option (:full-clone t)))
        (sly-el-indent . (:repo "cireu/sly-el-indent"))
        (cowboy-test . (:repo "casouri/cowboy-test"))
        (binder . (:repo "rnkn/binder"))
        (wgrep . (:repo "mhayashi1120/Emacs-wgrep"))
        (ghelp . (:repo "casouri/ghelp" :option (:full-clone t)))
        (valign . (:repo "casouri/valign" :option (:full-clone t)))
        (expand-region . (:repo "casouri/expand-region.el"
                                :option (:full-clone t)))
        (multi-translate . (:repo "twlz0ne/multi-translate.el"
                                  :dependency (bing-dict
                                               google-translate
                                               youdao-dictionary
                                               sdcv)))
        (deft . (:repo "casouri/deft" :option (:full-clone t)))))
