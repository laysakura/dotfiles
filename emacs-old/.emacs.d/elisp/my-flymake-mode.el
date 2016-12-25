;; NOTE!!
;; If you set 'check-syntax' rule in your Makefile,
;; you can use flymake with your compile options.
;;   ex) -std=c++0x

(require 'flymake)

;; color
(custom-set-faces
 '(flymake-errline ((((class color)) (:background "#882222"))))
 '(flymake-warnline ((((class color)) (:background "#5555aa"))))
 )

;; popup errors by popup.el
(defun my-flymake-display-err-popup.el-for-current-line ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (menu-data (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
        (popup-tip (mapconcat '(lambda (e) (nth 0 e))
                              (nth 1 menu-data)
                              "\n")))))

;; I'm not sure about it
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Makefile が無くてもC/C++のチェック
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))

(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-simple-generic-init cmd opts)))

(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

(push '("\\.[cC]\\'" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.[ch]pp\\'" flymake-cc-init) flymake-allowed-file-name-masks)

(setq flymake-mode-hook
      '(lambda ()
         (local-set-key "\M-p" 'flymake-goto-prev-error)
         (local-set-key "\M-n" 'flymake-goto-next-error)
         (local-set-key "\M-e" 'my-flymake-display-err-popup.el-for-current-line)
         ))
