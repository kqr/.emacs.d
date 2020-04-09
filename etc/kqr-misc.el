;;; kqr-misc.el --- Various useful snippets
;;; Commentary:
;;; Code:
(require 'subr-x)  ;; for hash-table-keys

(defun emacs-counter-occurrences ()
  "count the number of letters of each type is on the line (or in the region).
this is useful if you want to keep some sort of count, then you can press a
letter for each thing you're counting off (and different letters for differenc
classes of things) and at the end of it all ask emacs for a count of each thing.
ignores whitespace, so you can group characters or have them span multiple lines
or whatnot."
  (interactive)
  (save-excursion
    (let ((counts (make-hash-table))
          (begin (if (region-active-p)
                     (region-beginning)
                   (beginning-of-line)
                   (point)))
          (end (if (region-active-p)
                   (region-end)
                 (end-of-line)
                 (point))))
      (goto-char begin)
      (while (/= (point) end)
        (let ((item (following-char)))
          (unless (member (get-char-code-property item 'general-category) '(zs cc))
            (puthash item (1+ (gethash item counts 0)) counts)))
        (forward-char))
      (message (mapconcat (lambda (item) (format "%c: %s" item (gethash item counts)))
                          (hash-table-keys counts)
                          "    ")))))

(defun remove-all-advice-for (symbol)
  "Remove all advice for SYMBOL and return the number of advices removed."
  (interactive "a")
  (let ((removed-advices 0))
    (advice-mapc
     (lambda (advice props)
       (when advice
         (incf removed-advices)
         (advice-remove symbol advice)))
     symbol)
    (message "Removed %d pieces of advice" removed-advices)
    removed-advices))

;;; kqr-misc.el ends here
