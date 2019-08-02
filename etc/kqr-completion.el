(run-with-idle-timer
 6 nil
 (lambda ()
   (when (require 'company nil)
     (diminish 'company-mode)
     (global-company-mode +1)

     (defun company-complete-common-or-selected ()
       "Insert the common part, or if none, complete using selection."
       (interactive)
       (when (company-manual-begin)
         (if (not (equal company-common company-prefix))
             (company--insert-candidate company-common)
           (company-complete-selection))))

     (setq company-frontends
           '(company-pseudo-tooltip-unless-just-one-frontend
             company-preview-if-just-one-frontend
             company-echo-metadata-frontend))
     ;; Setting 0 idle delay was a nice idea, but it's way too slow for that
     ;; Someone reports 0.3 working okay with omnisharp too, but let's ramp
     ;; up slightly more slowly, to get maximum mileage out of it too.
     (setq company-idle-delay 0.1)
     (setq company-tooltip-align-annotations t)

     (when (require 'company-posframe)
       (company-posframe-mode +1))

     ;; We don't want completion to prevent us from actually navigating the code
     (define-key company-active-map (kbd "<return>") nil)
     (define-key company-active-map (kbd "C-p") nil)
     (define-key company-active-map (kbd "<up>") nil)
     (define-key company-active-map (kbd "C-n") nil)
     (define-key company-active-map (kbd "<down>") nil)
     (define-key company-active-map (kbd "C-<up>") #'company-select-previous)
     (define-key company-active-map (kbd "C-<down>") #'company-select-next)
     (define-key company-active-map (kbd "TAB")
       #'company-complete-common-or-cycle))))
