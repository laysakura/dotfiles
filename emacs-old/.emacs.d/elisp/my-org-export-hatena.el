(require 'org-export-hatena)

(add-hook 'org-mode-hook
          '(lambda()
             (local-set-key "\C-cH" 'org-export-hatena)))
