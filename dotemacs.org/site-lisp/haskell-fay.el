(defcustom haskell-fay-command "fay"
  "fay command to run")

(defcustom haskell-fay-process-name "fay"
  "fay process name")

(defcustom haskell-fay-buffer "*fay*"
  "fay process buffer")

(defun haskell-compile-fay ()
  "Initialize async fay compilation."
  (interactive)
  (progn
    (start-process-shell-command
          haskell-fay-process-name
          haskell-fay-buffer
          (format "%s %s"

                  haskell-fay-command
                  (buffer-file-name)))
    (set-process-sentinel
     (get-process haskell-fay-process-name)
     #'(lambda (process event)
         (cond
          ((string= event "finished\n")
           (alert (format "Fay OK %s\n%s"
                          event
                   (with-current-buffer (process-buffer process)
                     (buffer-substring (point-min) (point-max)))))
           (kill-buffer (process-buffer process)))
          ('t
           (alert (format "Fay Not OK\n%s"
                   (with-current-buffer (process-buffer process)
                     (buffer-substring (point-min) (point-max)))))
           (kill-buffer (process-buffer process))))))))

(provide 'haskell-fay)
