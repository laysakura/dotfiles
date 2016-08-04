(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-export-html character set
(setq org-export-html-coding-system 'utf-8)

;; use jditaa
;; (setq org-ditaa-jar-path "/home/sho/src/ditaa/trunk/web/lib/ditaa0_9.jar")

;; enable font-lock
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; color
(custom-set-faces '(org-block ((t (:inherit outline-3))))
                  '(org-drawer ((t (:foreground "magenta"))))
                  '(org-table ((t (:foreground "red"))))
                  )

;; hide '*'s at beggining of line
(setq org-hide-leading-stars t)

;;; org-babel
;; (require 'ob-ditaa)
;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; (setq org-babel-load-languages (quote ((emacs-lisp . t)
;;                                         (dot . t)
;;                                         (ditaa . t)
;;                                         (R . t)
;;                                         (python . t)
;;                                         (ruby . t)
;;                                         (gnuplot . t)
;;                                         (clojure . t)
;;                                         (sh . t)
;;                                         (ledger . t)
;;                                         (org . t))))
; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)


;; (require 'org-exp-bibtex)
(setq org-export-latex-date-format "%m/%d/%Y")
(setq org-beamer-frame-default-options "")
(setq org-export-latex-classes nil)

;;; Changes default latex headers
;; (setq org-export-latex-default-packages-alist
;;   '(("AUTO" "inputenc"  t)
;;     ("T1"   "fontenc"   t)
;;     (""     "fixltx2e"  nil)
;;     ("dvipdfmx"     "graphicx"  t)
;;     (""     "longtable" nil)
;;     (""     "float"     nil)
;;     (""     "wrapfig"   nil)
;;     (""     "soul"      t)
;;     (""     "textcomp"  t)
;;     (""     "marvosym"  t)
;;     (""     "wasysym"   t)
;;     (""     "latexsym"  t)
;;     (""     "amssymb"   t)
;;     (""     "hyperref"  nil)
;;     "\\tolerance=1000"
;;     ))
(setq org-export-latex-default-packages-alist nil)

;;; URL format in TeX
(setq org-export-latex-href-format "\\url{%s}")

;;; Changes image size to be inserted in TeX
(setq org-export-latex-image-default-option "width=40em")

;;; LaTeX listings support
(setq org-export-latex-listings t)

(add-to-list 'org-export-latex-classes
             '("jsarticle"
"
\\documentclass[a4j]{jsarticle}

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
%\\usepackage{fixltx2e}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{mediabb}
\\usepackage{longtable}
%\\usepackage{float}
%\\usepackage{wrapfig}
%\\usepackage{soul}
%\\usepackage{textcomp}
%\\usepackage{marvosym}
%\\usepackage{wasysym}
%\\usepackage{latexsym}
%\\usepackage{amssymb}
%\\usepackage{hyperref}
\\usepackage{listings,jlisting}
\\tolerance=1000
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               ))
(add-to-list 'org-export-latex-classes
             '("jreport"
"
\\documentclass[a4paper,11pt]{jreport}

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
%\\usepackage{fixltx2e}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{mediabb}
\\usepackage{longtable}
%\\usepackage{float}
%\\usepackage{wrapfig}
%\\usepackage{soul}
%\\usepackage{textcomp}
%\\usepackage{marvosym}
%\\usepackage{wasysym}
%\\usepackage{latexsym}
%\\usepackage{amssymb}
%\\usepackage{hyperref}
\\usepackage{listings,jlisting}
\\tolerance=1000
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               ))
(add-to-list 'org-export-latex-classes
             '("ipsjpapers"
"
\\documentclass{ipsjpapers}

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{mediabb}
\\usepackage{longtable}
\\usepackage{listings,jlisting}
\\tolerance=1000
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               ))
(add-to-list 'org-export-latex-classes
             '("ipsj"
"
\\documentclass[submit]{ipsj}

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{mediabb}
\\usepackage{longtable}
\\usepackage{listings,jlisting}
\\tolerance=1000
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               ))
(add-to-list 'org-export-latex-classes
             '("beamer"
"
\\documentclass[compress,dvipdfm]{beamer}
\\AtBeginDvi{\\special{pdf:tounicode EUC-UCS2}}
\\providecommand\\thispdfpagelabel[1]{}
\\usepackage{listings,jlisting}
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")
               ))

;; (defun org-insert-upheading (arg)
;;   (interactive "P")
;;   (org-insert-heading arg)
;;   (cond ((org-on-heading-p) (org-do-promote))
;;         ((org-at-item-p) (org-indent-item -1))) )

;; (defun org-insert-heading-dwim (arg)
;;   (interactive "P")
;;   (cond ((eq (car arg) 4) (org-insert-subheading nil)) ;C-u
;;         ((eq (car arg) 16) (org-insert-upheading nil)) ;C-u C-u
;;         (t (org-insert-heading nil))) )

(require 'auto-complete)
(require 'auto-complete-config)
(add-hook 'org-mode-hook
          '(lambda()
             (local-set-key "\C-cl" 'org-store-link)
             (local-set-key "\C-ca" 'org-agenda)
             (local-set-key "\C-cr" 'org-remember)
             ;; (local-set-key (kbd "C-return") 'org-insert-heading-dwim)  ; C-c RET

             (flyspell-mode)

             (auto-fill-mode -1)

             (when (require 'auto-complete nil t)
               (auto-complete-mode t))

             (if (not (string= (exec-path-from-shell-copy-env "MY_EMACS_MODE") "org2tex"))
                 (enable-paredit-mode))

             ;; Use \C-c\C-r as 'recentf-open-files
             (local-unset-key "\C-c\C-r")
             ))
