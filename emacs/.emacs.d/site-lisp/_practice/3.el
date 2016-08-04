(defun multiply-by-seven (num)
  "Multiply NUMBER by seven."
  (* 7 num))

(multiply-by-seven 7)

(if (> 5 4)
    (message "5 is greater than 4."))

(defun type-of-animal (characteristic)
  "Print a message depending on CHARACTERISTIC."
  (if (equal characteristic 'fierce)
      (message "tiger")
    (message "not a tiger")))

(type-of-animal 'fierc)

(if nil
    'true
  'false)

(let ((foo (buffer-name))
      (bar (buffer-size)))
  (message "This buffer is %s and is %d byte."
	   foo bar))

(message "Pointer is on %d th character in this buffer."
	 (- (point)
	    (save-excursion
	      (goto-char (point-min)) (point))))

(if (string= (int-to-string 24)
	     (substring (emacs-version) 10 12))
    (message "This version of Emacs is the latest.")
  (message "update to new version."))

(setq a '(sho haruka))
(setq b '(sho haruka))

(if (equal a b)
    (message "a == b")
  (message "a != b"))

(defun double (num)
  "Double NUM."
  (interactive "p")
  (message "%d * 2 = %d"
	   num (* num 2)))

(double 3)

(defun check (num)
  "check if NUM is greater than fill-column."
  (if (> fill-column num)
      (message "%d > fill-column" num)
    (message "otherwise")))

(check 7)

(defun simple ()
  "Move point to the beginning."
  (interactive)
  (push-mark)
  (goto-char (point-min)))


