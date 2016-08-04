(defalias 'perl-mode 'cperl-mode)

(setq auto-mode-alist
      (cons '("\\.\\(?:pl\|PL\|pm\\)$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))  ; ugly...

;; auto-complete

;; 'ac-define-source is defined as macro
(require 'auto-complete)


;; flymake
(require 'flymake)

(defun perllib-check-path (lst lib-path)
  (let ((dir (car lst)) (lst (cdr lst)))
    (setf lib-path (concat lib-path "/" dir))
    (if lst
        (if (string= dir "lib")
            lib-path
          (perllib-check-path lst lib-path)))))

(defun set-perl5lib ()
  "insert path to PERL5LIB if its path includes 'lib' directory."
  (interactive)
  (setf path-list (cdr (split-string buffer-file-name "/"))
        lib-path (perllib-check-path path-list ""))
  (if lib-path
      (let ()
        (setenv "PERL5LIB" lib-path)
        (message (concat "PERL5LIB=" lib-path)))))

(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (set-perl5lib)
  (flymake-mode t))


;; auto-insert
(define-auto-insert "\\.pl$" "perl-template.pl")
(define-auto-insert "\\.pm$" "perl-module-template.pm")


(add-hook 'cperl-mode-hook
          '(lambda()
             (cperl-set-style "PerlStyle")

             (setq cperl-electric-keywords t)
             (setq cperl-hairy t)
             (setq cperl-indent-parens-as-block t)
             (setq cperl-close-paren-offset -4)
             (setq cperl-continued-statement-offset 4)

             (flymake-perl-load)

             (local-unset-key "\C-h")
             (local-set-key "\C-j" 'newline-and-indent)
             ))
