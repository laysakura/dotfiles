(defun other-window-backward (&optional count)
  "Select another window in back-cyclic ordering of windows.
COUNT specifies the number of windows to skip, starting with the
selected window, before making the selection.  If COUNT if
positive, skip COUNT windows backwards.  If COUNT is negative,
skip -COUNT windows forewards.  COUNT zero means do not skip any
window, so select the selected window."
  (interactive "P")
  (other-window (- (prefix-numeric-value count))))

(defun expand-frame-width (&optional cols)
  "Expand the frame to the right in COLS columns (1 by default)."
  (interactive "P")
  (set-frame-width (selected-frame)
                   (+ (frame-width) (prefix-numeric-value cols))))
(defun shrink-frame-width (&optional cols)
  "Shrink the frame to the left in COLS columns (1 by default)."
  (interactive "P")
  (set-frame-width (selected-frame)
                   (- (frame-width) (prefix-numeric-value cols))))
(defun expand-frame-height (&optional rows)
  "Expand the frame below in ROWS rows (1 by default)."
  (interactive "P")
  (set-frame-height (selected-frame)
                    (+ (frame-height) (prefix-numeric-value rows))))
(defun shrink-frame-height (&optional rows)
  "shrink the frame above in ROWS rows (1 by default)."
  (interactive "P")
  (set-frame-height (selected-frame)
                    (- (frame-height) (prefix-numeric-value rows))))

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

(defun wrap-region-by-string ()
  (interactive
   (let ((start-point (region-beginning))
         (end-point (region-end))
         (start-str (read-string "Start: " nil 'my-history))
         (end-str (read-string "End: " nil 'my-history)))
     (save-excursion
       (goto-char start-point)
       (insert start-str)
       (goto-char (+ end-point (length start-str)))
       (insert end-str)))))

(defmacro setq-local (var val )
  `(set (make-local-variable 'var) ,val))
