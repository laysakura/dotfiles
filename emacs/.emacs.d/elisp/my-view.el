(setq inhibit-startup-message t)

(load-theme 'monokai t)
(menu-bar-mode -1)

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "color-167"))))

 '(flycheck-warning ((t (:background "color-58") (:foreground "color-255"))))
 '(flycheck-fringe-warning ((t (:background "color-58") (:foreground "color-255"))))
)

;; Color
(progn
  ;; モードラインの文字の色を設定します。
  (set-face-foreground 'mode-line "#ffffff")
  ;; モードラインの背景色を設定します。
  (set-face-background 'mode-line "color-20")
  ;; モードライン（アクティブでないバッファ）の文字色を設定します。
  (set-face-foreground 'mode-line-inactive "white")
  ;; モードライン（アクティブでないバッファ）の背景色を設定します。
  (set-face-background 'mode-line-inactive "color-17")
)
