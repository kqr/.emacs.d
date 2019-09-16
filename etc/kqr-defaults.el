(defun platform-specific (default &optional osx)
  "Pick a platform-specific value for e.g. OSX, or use DEFAULT."
  (or (when (eq system-type 'darwin) osx)
      default))

(setq-default
 inhibit-startup-screen t
 initial-scratch-message ""

 major-mode 'fundamental-mode
 make-backup-files nil
 large-file-warning-threshold 100000000

 ;; Reduces lag, I think
 auto-window-vscroll nil
 line-move-visual nil

 ;; Don't complain about not being able to scroll below end of file
 scroll-error-top-bottom t

 ;; Try to keep cursor centered to the extent possible with vanilla Emacs
 scroll-preserve-screen-position t
 scroll-conservatively 0
 maximum-scroll-margin 0.5
 scroll-margin 99999

 ;; No need to fake typesetting.
 sentence-end-double-space nil

 ;; Hide cursor in inactive windows
 cursor-in-non-selected-windows nil

 ;; Set a column limit at 80 characters
 fill-column 80
 ;; Automatically hard wrap content instead
 auto-fill-function 'do-auto-fill
 ;; Create new lines when moving past end of buffer
 next-line-add-newlines t

 ;; Copy stuff to the X11 primary selection
 select-enable-primary t
 ;; Typing stuff with active region replaces region
 delete-selection-mode 1

 ;; Focus on newly opened help windows
 help-window-select t

 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program (platform-specific "firefox" "open_firefox")

 ;; Let text extend beyond the window width
 truncate-lines t

 ;; Prevent Emacs from mixing tabs and spaces.
 indent-tabs-mode nil)

;; Replace the default line-extends-beyond-window symbol
(set-display-table-slot standard-display-table 0 ?›)

;; C-z defaults to suspend-frame which behaves weirdly and is never necessary
(define-key global-map (kbd "C-z") nil)

;; This is neat to quickly go back to the previous buffer
(define-key global-map (kbd "C-q") #'kill-this-buffer)
;; But then we also need this...
(define-key global-map (kbd "C-S-q") #'quoted-insert)

;; Make "join this line to the one above" a bit more convenient to perform
(define-key global-map (kbd "C-S-j") #'delete-indentation)

(when (require 'drag-stuff)
  (drag-stuff-global-mode +1)
  (drag-stuff-define-keys))
