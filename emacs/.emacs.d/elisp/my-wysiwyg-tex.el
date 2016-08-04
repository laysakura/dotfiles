(eval-after-load 'cl
  '(require 'wysiwyg-tex))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (setq wysiwyg-tex-tex2dvi-command "platex"
                   wysiwyg-tex-using-color-package t
                   wysiwyg-tex-typeset-3-times t
                   wysiwyg-tex-doc-view-fit-preview 1
                   wysiwyg-tex-extract-page-around 10)
             (local-set-key "\C-c\C-p" 'wysiwyg-tex-show-preview)
             (local-set-key "\C-cp" 'wysiwyg-tex-show-whole-preview)))
