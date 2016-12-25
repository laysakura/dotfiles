;; eval common-lisp package for elisp before byte-compile
(eval-when-compile (require 'cl))

;; text encoding
(coding-system-put 'utf-8 'category 'utf-8)
(set-language-info "Japanese" 'coding-priority(cons 'utf-8(get-language-info "Japanese" 'coding-priority)))
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Dicrease GC
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; Increase logs
(setq message-log-max 10000)

;; Increase history
(setq history-length 1000)

;; Faster display of key stroke
(setq echo-keystrokes 0.05)

;; Allow recursive mini-buffer call
;; (setq enable-recursive-minibuffers t)

;; Use message-buffer instead of dialog
(setq use-dialog-box nil)
(defalias 'message-box 'message)

(byte-recompile-directory ELISP_DIR)
