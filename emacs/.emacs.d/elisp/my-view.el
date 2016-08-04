;; set frame attributes
(setq default-frame-alist
      (append (list
               '(width . 80)
               '(height . 65)
               '(top . 17)
               '(left . 0)
               ;; '(alpha . (80 80 30 30))
               )
              initial-frame-alist)
      frame-title-format "%b - Emacs"
      inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-no-scroll-bar)

;; set color theme
(require 'color-theme)
(color-theme-dark-laptop)

;; font setting
(add-to-list 'default-frame-alist
             '(font . "-unknown-Inconsolata-normal-normal-normal-*-11-*-*-*-*-0-iso10646-1"))
