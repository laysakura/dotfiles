(require 'erlang-start)

(setq auto-mode-alist
      (cons '("\\.erlang$" . erlang-mode) auto-mode-alist))

(add-hook 'erlang-mode-hook
          '(lambda ()
             ))
