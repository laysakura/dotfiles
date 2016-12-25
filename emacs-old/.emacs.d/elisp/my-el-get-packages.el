(setq el-get-dir "~/.emacs.d/site-lisp/el-get")
(setq el-get-github-default-url-type 'https)
(require 'el-get)

;; non-standard packages
(setq el-get-sources
      '(
        (:name kolon-mode
               :type github
               :pkgname "samvtran/kolon-mode"
               :description "kolon-mode - Syntax highlighting for Xslate files using the kolon syntax")
        (:name markdown-mode
               :type github
               :pkgname "defunkt/markdown-mode"
               :description "Major mode to edit Markdown files in Emacs")
        ;; (:name django-mode
        ;;        :type github
        ;;        :pkgname "myfreeweb/django-mode"
        ;;        :description "Django mode and snippets for Emacs")
        (:name haml-mode
               :type github
               :pkgname "nex3/haml-mode"
               :description "Emacs mode for Haml")
        ;; (:name yasnippet
        ;;        :website "https://github.com/capitaomorte/yasnippet.git"
        ;;        :description "YASnippet is a template system for Emacs."
        ;;        :type github
        ;;        :pkgname "capitaomorte/yasnippet"
        ;;        :features "yasnippet"
        ;;        :compile "yasnippet.el")
        (:name plantuml-mode
               :type github
               :pkgname "zwz/plantuml-mode"
               :description "PlantUML mode for Emacs")
        (:name ruby-electric
               :type github
               :pkgname "qoobaa/ruby-electric"
               :description "Improved ruby-electric mode")
        (:name ruby-block
               :type github
               :pkgname "adolfosousa/ruby-block.el"
               :description "Emacs highlight matching block")
        (:name inf-ruby
               :type github
               :pkgname "nonsequitur/inf-ruby"
               :description "inf-ruby provides a REPL buffer connected to a Ruby subprocess.")
        (:name robe
               :type github
               :pkgname "dgutov/robe"
               :description "Code navigation, documentation lookup and completion for Ruby")
        (:name s.el
               :type github
               :pkgname "magnars/s.el"
               :description "The long lost Emacs string manipulation library.")
        (:name dash.el
               :type github
               :pkgname "magnars/dash.el"
               :description "A modern list library for Emacs")
        (:name f.el
               :type github
               :pkgname "rejeep/f.el"
               :description "Modern API for working with files and directories in Emacs.")
        (:name bison-mode
               :type github
               :pkgname "Wilfred/bison-mode"
               :description "Emacs major mode for Bison, Yacc and Lex grammars")
        (:name js2-mode
               :type github
               :pkgname "mooz/js2-mode"
               :description "Improved JavaScript editing mode for GNU Emacs")
        (:name ac-js2
               :type github
               :pkgname "ScottyB/ac-js2"
               :description "Javascript auto-completion in Emacs using Js2-mode's parser and Skewer-mode.")
        (:name skewer-mode
               :type github
               :pkgname "skeeto/skewer-mode"
               :description "Live web development in Emacs")
        (:name emacs-web-server
               :type github
               :pkgname "skeeto/emacs-web-server"
               :description "Extensible Emacs HTTP 1.1 server")
        ))


;;; special care for older emacs
(if (<= emacs-major-version 23)

    (setq el-get-sources (append
          '(
            (:name python
                   :type github
                   :pkgname "fgallina/python.el"
                   :description "python.el for emacs23"
                   :branch "emacs23")
            )
          el-get-sources)))


;; Packages to install from el-get
(defvar my/el-get-packages
  '(
    jedi
    python
    markdown-mode
    ;; django-mode
    haml-mode
    exec-path-from-shell
    kolon-mode
    yaml-mode
    cl-lib
    ;; yasnippet
    plantuml-mode
    ruby-electric
    ruby-block
    inf-ruby
    robe
    s.el
    dash.el
    f.el
    bison-mode
    js2-mode
    ac-js2
    skewer-mode
    emacs-web-server
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync my/el-get-packages)
