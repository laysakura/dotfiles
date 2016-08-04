(require 'twittering-mode)

(setq twittering-auth-method 'xauth
      twittering-username "laysakura")

(add-hook 'twittering-mode-hook
          '(lambda ()
             (twittering-icon-mode 1)
             ))

