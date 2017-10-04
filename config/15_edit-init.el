
(defun edit-init ()
  "Interactive command to open the .emacs init file."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))
