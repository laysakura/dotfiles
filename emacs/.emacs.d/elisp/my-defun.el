(defun other-window-backward (&optional count)
  "Select another window in back-cyclic ordering of windows.
COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT if
positive, skip COUNT windows backwards.  If COUNT is negative,
skip -COUNT windows forewards.  COUNT zero means do not skip any
window, so select the selected window."
  (interactive "P")
  (other-window (- (prefix-numeric-value count))))

(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "Display selected buffer in current window"
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))
