;; Update
;; (auto-install-from-url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el")
(require 'hideshow)
(require 'fold-dwim)

(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-interaction-mode-hook 'hs-minor-mode)
(add-hook 'haskell-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'ruby-mode-hook 'hs-minor-mode)
(add-hook 'yatex-mode-hook 'hs-minor-mode)
