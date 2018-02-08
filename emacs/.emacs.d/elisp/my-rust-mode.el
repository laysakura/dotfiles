(add-hook 'rust-mode-hook
          (lambda ()
            (setq flycheck-checker 'cargo)))

(require 'company-racer)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
