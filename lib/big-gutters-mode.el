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

(defcustom bgm-ignore-mode-regex "dired|org-agenda"
  "Prevent big gutters in these modes."
  :group 'big-gutters
  :tag "Ignore Mode Regex"
  :type 'string)

;;;###autoload
(define-minor-mode big-gutters-mode
  "Make window margins big to get a reasonable line width."
  :init-value nil
  :lighter " GUT"
  (cond (big-gutters-mode
         (bgm-update)
         (add-hook 'window-configuration-change-hook 'bgm-update t t))
        (t
         (bgm-update t)
         (remove-hook 'window-configuration-change-hook 'bgm-update t))))

(define-global-minor-mode global-big-gutters-mode
  big-gutters-mode bgm-toggle-with-ignore)

(defun bgm-toggle-with-ignore ()
  "Toggle big gutters globally except when not applicable."
  (if big-gutters-mode
      (big-gutters-mode -1)
    (unless (string-match bgm-ignore-mode-regex (symbol-name major-mode))
      (big-gutters-mode +1))))

(defun bgm-update (&optional disable)
  "Recalculate correct gutter widths.  If DISABLE, disable big gutters."
  (interactive)
  (dolist (window (window-list))
    (unless (or
             (not (with-selected-window window big-gutters-mode))
             (window-minibuffer-p window))
      (let* ((target (or (with-selected-window window bgm-line-width) 120))
             (fringes (apply #'+(seq-filter #'numberp
                                            (window-fringes window))))
             (width (+ (window-width window) (/ fringes (frame-char-width))))
             (extra-chars (- width target))
             (margins (* (frame-char-width) extra-chars)))
        (if (or disable (< margins 1))
            (set-window-fringes window nil nil)
          (set-window-fringes window
                              (truncate (* 0.25 margins))
                              (truncate (* 0.75 margins))))))))

(provide 'big-gutters-mode)
;;; big-gutters-mode.el ends here
