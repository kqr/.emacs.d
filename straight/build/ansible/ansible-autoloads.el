;;; ansible-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ansible" "ansible.el" (0 0 0 0))
;;; Generated autoloads from ansible.el

(defvar ansible-key-map (make-sparse-keymap) "\
Keymap for Ansible.")

(autoload 'ansible "ansible" "\
Ansible minor mode.

\(fn &optional ARG)" t nil)

(autoload 'ansible-dict-initialize "ansible" "\
Initialize Ansible auto-complete.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ansible" '("ansible-")))

;;;***

(provide 'ansible-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ansible-autoloads.el ends here
