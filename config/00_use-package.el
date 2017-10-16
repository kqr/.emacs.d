(require 'package)
(push '("marmalade" . "https://marmalade-repo.org/packages/") package-archives)
(push '("melpa stable" . "https://stable.melpa.org/packages/") package-archives)
(push '("melpa" . "https://melpa.milkbox.net/packages/") package-archives)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering :init
  (setq backup-inhibited t)
  (setq auto-save-default nil))
