(defun kill-word (arg)
  "Do what I mean and don't kill the word if there is whitespace to kill..."
  (interactive "P")
  (kill-region (point) (progn (forward-same-syntax arg) (point))))

(defun kill-region-or-word-dwim (arg)
  "Either kill the active region or kill a word forward"
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-word arg)))

(bind-key "C-w" 'kill-region-or-word-dwim)
(bind-key* "DEL" 'backward-kill-word)
(eval-after-load "paredit" #'(bind-key* "DEL" 'paredit-backward-kill-word))
