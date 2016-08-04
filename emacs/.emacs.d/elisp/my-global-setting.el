;; iswitchb
(iswitchb-mode 1)

;; display directory name of buffer when two or more buffers with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; always maintain last 1 null line
(setq require-final-newline 'visit-save)

;; display (row, colmun)
(setq line-number-mode 1)
(setq column-number-mode 1)

;; flash screen rather than beep
(setq visible-bell t)

;; highlight parens pair
(show-paren-mode 1)

;; grep
(setq grep-command "grep -nH -r -i -e ")


;; share clipboard w/ Window systems
(if (eq system-type 'darwin)
    (progn
      (defun copy-from-osx ()
        (shell-command-to-string "pbpaste"))

      (defun paste-to-osx (text &optional push)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (setenv "__CF_USER_TEXT_ENCODING" "0x1F5:0x8000100:14")
            (process-send-string proc text)
            (process-send-eof proc))))

      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx)))


(if (eq system-type 'gnu/linux)
    (progn
      (defun my-cut-function (text &optional rest)
        (interactive)
        (let ((process-connection-type nil))
          (let ((proc (start-process "xclip" "*Messages*" "xclip")))
            (process-send-string proc text)
            (process-send-eof proc))))

      (defun my-paste-function ()
        (interactive)
        (shell-command-to-string "xclip -o"))

      (when (and (not window-system)
                 (executable-find "xclip"))
        (setq interprogram-cut-function 'my-cut-function)
        (setq interprogram-paste-function 'my-paste-function))))


;; add colors to 'Tab', 'Zenkaku-Space', and white-space at end of lines
(defface my-face-b-1 '((t (:background "light grey"))) nil)
(defface my-face-b-2 '((t (:background "tomato"))) nil)
(defface my-face-u-1 '((t (:underline "firebrick1"))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defface my-face-b-01 '((t (:foreground "#eebbbb" :underline t))) nil)
(defface my-face-b-02 '((t (:foreground "#bbeebb" :underline t))) nil)
(defface my-face-b-03 '((t (:foreground "#bbbbee" :underline t))) nil)
(defface my-face-b-04 '((t (:foreground "#ebbbbe" :underline t))) nil)
(defface my-face-b-05 '((t (:foreground "#bbbeeb" :underline t))) nil)
(defface my-face-b-06 '((t (:foreground "#ffdead" :underline t))) nil)
(defface my-face-b-07 '((t (:foreground "#beafff" :underline t))) nil)
(defface my-face-b-08 '((t (:foreground "LightPink" :underline t))) nil)
(defface my-face-b-09 '((t (:foreground "CadetBlue1" :underline t))) nil)
(defface my-face-b-10 '((t (:foreground "LightGoldenrod" :underline t))) nil)
(defface my-face-b-11 '((t (:foreground "MediumOrchid1" :underline t))) nil)
(defface my-face-b-12 '((t (:foreground "orange1" :underline t))) nil)
(defvar my-face-b-01 'my-face-b-01)
(defvar my-face-b-02 'my-face-b-02)
(defvar my-face-b-03 'my-face-b-03)
(defvar my-face-b-04 'my-face-b-04)
(defvar my-face-b-05 'my-face-b-05)
(defvar my-face-b-06 'my-face-b-06)
(defvar my-face-b-07 'my-face-b-07)
(defvar my-face-b-08 'my-face-b-08)
(defvar my-face-b-09 'my-face-b-09)
(defvar my-face-b-10 'my-face-b-10)
(defvar my-face-b-11 'my-face-b-11)
(defvar my-face-b-12 'my-face-b-12)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ;; indentation
     ;; ("^                        " 0 my-face-b-12 append)
     ;; ("^                      " 0 my-face-b-11 append)
     ;; ("^                    " 0 my-face-b-10 append)
     ;; ("^                  " 0 my-face-b-09 append)
     ;; ("^                " 0 my-face-b-08 append)
     ;; ("^              " 0 my-face-b-07 append)
     ;; ("^            " 0 my-face-b-06 append)
     ;; ("^          " 0 my-face-b-05 append)
     ;; ("^        " 0 my-face-b-04 append)
     ;; ("^      " 0 my-face-b-03 append)
     ;; ("^    " 0 my-face-b-02 append)
     ;; ("^  " 0 my-face-b-01 append)

     ;; useless white spaces
     ("\t" 0 my-face-u-1 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-b-2 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; default indentation settings
(setq-default indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4)

;; Save cursor place for next session
(setq-default save-place t)
(require 'saveplace)

;; Use 'y' instead of 'yes'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable auto-insert-mode
(auto-insert-mode)
(setq auto-insert-directory (concat ELISP_DIR "/" "auto-insert"))

;; 1/3 split window horizontally (C-x#)
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(global-set-key "\C-x#" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))
