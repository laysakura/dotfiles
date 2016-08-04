;;; jisho.el -- English/Japanese dictionary for Emacs

;; Copyright (C) 2011 Sho Nakatani

;; Author: Sho Nakatani <lay.sakura@gmail.com>
;; Keywords: english japanese dictionary jisho

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; jisho.el is (English->Japanese | Japanese->English) dictionary
;; working on Emacs. This software makes use of SPACE ALC (http://www.alc.co.jp/)
;; database. Even if you get frustrated by using this software,
;; NEVER REPORT TO ALC, rather tell the author what bothers you :-)
;; This software also uses deferred.el. Big Thanks to Mr. SAKURAI MAsashi :-D

;;; Requirement:
;; Internet connection

;;; Installation:
;;
;; 1. Extract archive.
;;     $ tar xvf jisho-el.tar.gz
;;
;; 2. Put 'jisho-el' directory to somewhere and cd to it.
;;    For example:
;;     $ mv jisho-el ~/lib
;;     $ cd ~/lib/jisho-el
;;
;; 3. byte-comple jisho.el
;;     $ make
;;
;; 4. Create a symbolic link in a directory with PATH
;;     # make install
;;   or
;;     $ chmod u+x main.py
;;     # ln -s `pwd`/main.py /usr/local/bin/jisho
;;
;; 5. Put jisho.el and diferred.el into a directory with load-path.
;;    For Example:
;;     $ cp *.el *.elc ~/.emacs.d/
;;
;; 6. Add following to .emacs:
;;     (require 'jisho)

;;; Usage:
;;
;; 1. M-x jisho-show-gradually [RET]
;;    Then input what you want to translate.
;;    Either English and Japanese are supported
;;
;; 2. Just evaluate S expression.
;;    (jisho-show-gradually "what you want")
;;    (jisho-show-gradually "調べ物")

(eval-when-compile
  (require 'cl))

(require 'deferred)

(defun jisho-show-gradually (&optional query)
  "Lookup English/Japanese dictionary and show results in a new buffer."
  (interactive)
  (save-current-buffer
    (save-excursion
      (lexical-let* ((q (if (null query)
                            (read-string (format "Search: ") "")
                          query))
                     (page 1)
                     (maxpage (string-to-number
                               (shell-command-to-string
                                (concat "jisho --ask-num-pages "
                                        q))))
                     (wait-time 1000))
        (set-buffer (get-buffer-create "*jisho result*"))
        (erase-buffer)
        (display-buffer "*jisho result*")
        (deferred:$
          (deferred:next
            (deferred:lambda (x)
              (set-buffer (get-buffer-create "*jisho result*"))
              (save-excursion
                (goto-char (buffer-end 1))
                (insert (shell-command-to-string
                         (concat "jisho -p"
                                 (format "%s " page)
                                 q))) )
              (if (< (incf page) maxpage)
                  (deferred:nextc (deferred:wait wait-time) self)) ))))))
  t)

(provide 'jisho)
