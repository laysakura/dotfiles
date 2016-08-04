;; Update
;; (auto-install-from-url "http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el")

(require 'auto-save-buffers)
(run-with-idle-timer 5 t 'auto-save-buffers)
