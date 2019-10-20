;; Fixed width font
(when (display-graphic-p)
  (set-frame-font (font-spec :name "Input Sans Light" :size 14) t t)
  (custom-theme-set-faces 'user '(fixed-pitch
                                  ((t :family "Input Sans Light"
                                      :height 1.0))))  ;; was Luxi Mono, then Hack
  (custom-theme-set-faces 'user '(variable-pitch
                                  ((t :family "Linux Libertine O"
                                      :height 1.3))))
  (add-to-list 'initial-frame-alist '(line-spacing . 1))
  (add-to-list 'default-frame-alist '(line-spacing . 1)))

;; Set variable width font in text buffers
(add-hook 'text-mode-hook 'variable-pitch-mode)


(when (require 'face-remap nil)
  ;; Make available smaller changes in text size
  (setq-default text-scale-mode-step 1.05)
  (define-key f11-map (kbd "f") 'toggle-frame-fullscreen)
  (define-key f11-map (kbd "z") 'text-scale-adjust)

  ;; Set fixed-width fonts where needed
  (setq-default buffer-face-mode-face '(:inherit fixed-pitch))
  (add-hook 'calendar-mode-hook #'buffer-face-mode)
  (add-hook 'notmuch-tree-mode-hook #'buffer-face-mode))


;; Retain ANSI colour sequences in things like compilation buffers
(when (require 'ansi-color nil)
  (defun ansi-coloured-buffer ()
    "Interpret ANSI colour sequences correctly in current buffer."
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'ansi-coloured-buffer))


;; Dim inactive buffers
(when (require 'dimmer)
  (dimmer-mode +1)
  (setq dimmer-fraction 0.3))
