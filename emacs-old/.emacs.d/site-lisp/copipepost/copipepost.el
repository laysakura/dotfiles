(defun replace-string-in-buffer (from to)
  (save-excursion
	(goto-char (point-min))
	(while (search-forward from nil t)
	  (replace-match to nil t))))

(defun copipepost ()
  "Process Copipe texts."
  (interactive)

  (replace-string-in-buffer "、" "，")
  (replace-string-in-buffer "。" "．"))
