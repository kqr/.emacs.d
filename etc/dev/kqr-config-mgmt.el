;; Ansible mode
(autoload 'ansible "ansible")
(add-hook 'yaml-mode-hook 'ansible)
(with-eval-after-load "ansible"
  ;; The easiest solution to customise this on various work laptops is to
  ;; symlink the actual password file to this location – I'm unlikely to manage
  ;; more than one set of Ansible-controlled hosts anyway.
  (setq ansible-vault-password-file "~/.vault_pass")
  (add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt))

;; CFEngine mode
(autoload 'cfengine3-mode "cfengine")
(push '("\\.cf\\'" . cfengine3-mode) auto-mode-alist)
(with-eval-after-load "cfengine"
  (add-hook 'cfengine3-mode-hook 'eldoc-mode))
