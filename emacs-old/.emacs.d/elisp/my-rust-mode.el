(add-hook 'rust-mode-hook
          (lambda ()
            (setq flycheck-checker 'cargo)))
; setup company-racer
(require 'company-racer)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))
; hooks
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'rustfmt-enable-on-save)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
