(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; dictionary
(add-to-list 'ac-dictionary-directories (concat ELISP_DIR "/auto-complete/dict"))
(add-to-list 'ac-user-dictionary-files (concat ELISP_DIR "/auto-complete/dict/user-dict"))
(setq ac-sources (append ac-sources))

;; key bindings
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key ac-completing-map "\C-n" 'ac-next)
(define-key ac-completing-map "\C-p" 'ac-previous)

;; design
(set-face-background 'ac-candidate-face "gainsboro")
(set-face-background 'ac-selection-face "dark slate blue")
(set-face-background 'ac-completion-face "dark slate blue")

;; tiny settings
(setq ac-ignore-case t
      ac-auto-show-menu t
      ac-candidate-limit 50)

;; Enable for all buffers
(global-auto-complete-mode 1)

;; using look command
(defun my-ac-look ()
  "look コマンドの出力をリストで返す"
  (interactive)
  (unless (executable-find "look")
    (error "look コマンドがありません"))
  (let ((search-word (thing-at-point 'word)))
    (with-temp-buffer
      (call-process-shell-command "look" nil t 0 search-word)
      (split-string-and-unquote (buffer-string) "\n"))))
(defun ac-complete-look ()
  (interactive)
  (let ((ac-menu-height 50)
        (ac-candidate-limit t))
  (auto-complete '(ac-source-look))))
(defvar ac-source-look
  '((candidates . my-ac-look)
    (requires . 2)))  ;; 2文字以上ある場合にのみ対応させる
(global-set-key (kbd "M-'") 'ac-complete-look)
