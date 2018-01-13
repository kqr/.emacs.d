;;; simpler-centered-cursor-mode.el --- A more basic centered cursor mode
;;; Commentary:
;;
;; This was born out of my frustration with some unfortunate combination of Org
;; and the old centered-cursor-mode.el, which caused insane amounts of slowness
;; when navigating the buffer. I looked at the code to see if I could improve
;; something, and discovered how complicated centered-cursor-mode.el really is.
;; A lot of it is nice stuff related to animation and such, but it made it
;; really hard for me to try to optimise.
;;
;; This mode is basically just a hook that starts an idle timer that, when
;; fired, will call the Emacs built-in (recenter) procedure.
;;
;;; Code:

;; TODO: handle mouse wheel. rebind to next-line?
;; TODO: handle page up/page down

(defgroup simpler-centered-cursor nil
  "Scroll the document so as to keep the cursor centered."
  :group 'scrolling
  :group 'convenience)

(defcustom scc-idle-time 0.2
  "Time user has to be idle before recentering is triggered.
There are essentially two values of interest here: longer or
shorter than your keyboard repeat rate.

If your keyboard repeat rate (i.e. how fast keyboard events are
triggered when a key is held down) is 0.2 seconds, then setting a
0.3 second idle time will allow Emacs to move the cursor around
freely, and only after the key is released will the recentering
occur. Setting a 0.1 second idle time will keep recentering, even
while you are still holding down a navigation key."
  :group 'simpler-centered-cursor
  :tag "Idle time"
  :type 'number)

(defvar scc-map
  (let ((scc-map (make-sparse-keymap)))
    (when (and (boundp 'mouse-wheel-mode) mouse-wheel-mode)
      (define-key scc-map (vector mouse-wheel-down-event) #'scc-mwheel)
      (define-key scc-map (vector mouse-wheel-up-event) #'scc-mwheel))
    scc-map))

(defun scc-mwheel (event)
  "Doc EVENT."
  (let ((button (mwheel-event-button event)))
    (cond
     ((eq button mouse-wheel-down-event)
      (forward-line (- 1)))
     ((eq button mouse-wheel-up-event)
      (forward-line 1)))))

(defun scc-recenter ()
  "If the active buffer is visible, scroll it to recenter the cursor."
  (unless (minibufferp (current-buffer))
    (when (equal (current-buffer) (window-buffer (selected-window)))
      (recenter))))

(defun scc-set-timer ()
  "Set a recentering idle timer."
  (run-with-idle-timer scc-idle-time nil #'scc-recenter))

;;;###autoload
(define-minor-mode simpler-centered-cursor-mode
  "Scroll the document so as to keep the cursor centered.."
  :init-value nil
  :lighter " ·"
  :keymap scc-map
  (cond (simpler-centered-cursor-mode
         (add-hook 'post-command-hook 'scc-set-timer t t)
         (add-hook 'window-configuration-change-hook 'scc-set-timer t t))
        (t
         (remove-hook 'post-command-hook 'scc-set-timer t)
         (remove-hook 'window-configuration-change-hook 'scc-set-timer t))))

(define-global-minor-mode global-simpler-centered-cursor-mode
  simpler-centered-cursor-mode simpler-centered-cursor-mode)

(provide 'simpler-centered-cursor-mode)
;;; simpler-centered-cursor-mode.el ends here
