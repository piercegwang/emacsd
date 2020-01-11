(defun elpy-snippet-docstring-assignments (arg-string)
  "Return the typical docstring assignments for arguments in reST documentation."
  (let ((indentation (make-string (save-excursion
                                    (goto-char start-point)
                                    (current-indentation))
                                  ?\s)))
    (mapconcat (lambda (arg)
                 (if (string-match "^\\*" (car arg))
                     ""
                   (format ":param %s: \n%s"
                           (car arg)
                           indentation)))
               (elpy-snippet-split-args arg-string)
               "")))
