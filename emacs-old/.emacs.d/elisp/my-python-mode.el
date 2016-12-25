;; fgallina/python.el
(require 'python)

;; jedi
(require 'deferred)
(setq jedi:setup-keys t)
(require 'jedi)

;; auto-complete

;; 'ac-define-source is defined as macro
(require 'auto-complete)

(defun ac-python-candidates ()
  (python-find-imports)
  (car (read-from-string
        (python-send-receive
         (format "emacs.complete(%S,%s)"
                 (python-partial-symbol)
                 python-imports)))))

;; flymake
(require 'flymake)

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "\pyflakes-with-pep8" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))


;; auto-insert
(define-auto-insert "\\.py$" "python-template.py")


(add-hook 'python-mode-hook
          '(lambda()
             ;; jedi
             (jedi:setup)
             (jedi:ac-setup)

             (flymake-mode 1)
             ))
