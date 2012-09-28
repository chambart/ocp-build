(message "Loading ocp-fix-errors.el...")

(defconst ocp-fix-errors-program "ocp-fix-errors.sh"
  "command for invoking ocp-fix-errors")

(defun ocp-fix-errors ()
  "automatically fix errors in compilation buffer"
  (interactive)
;;  (setq debug-on-error t)
  (set-buffer (get-buffer-create "*compilation*"))
  (let ((line-num (number-to-string (line-number-at-pos (point)))))
    (let ((temp-file (make-temp-file "ocpfix")))
      (write-region nil nil temp-file nil 'x nil)
      (with-temp-buffer
        (insert (shell-command-to-string
                 (concat ocp-fix-errors-program " " temp-file " " line-num)))
      (eval-buffer))
    (delete-file temp-file)
    ))
)

(message "Loaded ocp-fix-errors-program")

