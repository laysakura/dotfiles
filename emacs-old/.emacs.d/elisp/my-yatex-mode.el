(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))

(setq tex-command "export TEXMFHOME=/home/sho/share/texmf ; latexmk -dvi"
      dvi2-command "acroread"
      YaTeX-kanji-code 4)

;; auto-complete
;; (require 'auto-complete-latex)
;; (setq ac-l-dict-directory (concat SITE_LISP_DIR "/auto-complete-latex/ac-l-dict/"))
;; (add-to-list 'ac-modes 'yatex-mode)

;; flymake
(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-g0" "-r" "-l" (expand-file-name "~/.chktexrc") "-I" "-q" "-v0" file-name)))
(push
 '("^\\(\.+\.tex\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)

(add-hook 'yatex-mode-hook
          '(lambda ()
             (flymake-mode 1)
             ;; (ac-l-setup)
             ))


