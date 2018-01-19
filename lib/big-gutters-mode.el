;;; big-gutters-mode.el --- Typographically sound visible line widths.
;;; Commentary:

;;; Code:

(defgroup big-gutters nil
  "Doc."
  :group 'appearance
  :group 'typography)

(defcustom bgm-line-width 80
  "Doc."
  :group 'big-gutters
  :tag "Line Width"
  :type 'integer)

(defcustom bgm-ignore-mode-regex "dired\\|org-agenda"
  "Prevent big gutters in these modes."
  :group 'big-gutters
  :tag "Ignore Mode Regex"
  :type 'string)

;;;###autoload
(define-minor-mode big-gutters-mode
  "Make window margins big to get a reasonable line width."
  :init-value nil
  :lighter " GUT"
  (make-local-variable 'window-configuration-change-hook)
  (cond (big-gutters-mode
         (add-hook 'window-configuration-change-hook 'bgm-update t t)
         (bgm-update))
        (t
         (bgm-update))))

(define-global-minor-mode global-big-gutters-mode
  big-gutters-mode bgm-toggle-with-ignore)

(defun bgm-toggle-with-ignore ()
  "Toggle big gutters globally except when not applicable."
  (if (or big-gutters-mode (string-match bgm-ignore-mode-regex
                                         (symbol-name major-mode)))
      (big-gutters-mode -1)
    (big-gutters-mode +1)))

(defun bgm-update ()
  "Recalculate correct gutter widths.  If DISABLE, disable big gutters."
  (if (or (window-minibuffer-p) (not big-gutters-mode))
      (set-window-fringes (selected-window) nil nil)
    (let* ((fringes (apply #'+ (seq-filter #'numberp (window-fringes))))
           (width (+ (window-width) (/ fringes (frame-char-width))))
           (extra-chars (- width bgm-line-width))
           (margins (* (frame-char-width) extra-chars)))
      (when (> margins 0)
        (set-window-fringes (selected-window)
                            (truncate (* 0.25 margins))
                            (truncate (* 0.75 margins)))))))

(provide 'big-gutters-mode)
;;; big-gutters-mode.el ends here
