;; Ansible mode
(autoload 'ansible "ansible")
(add-hook 'yaml-mode-hook 'ansible)
(with-eval-after-load "ansible"
  (setq ansible::vault-password-file "~/.vault_pass")
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))

;; CFEngine mode
(autoload 'cfengine3-mode "cfengine")
(push '("\\.cf\\'" . cfengine3-mode) auto-mode-alist)
(with-eval-after-load "cfengine"
  (add-hook 'cfengine3-mode-hook 'eldoc-mode))
