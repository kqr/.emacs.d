;;;; REST client mode
(require 'restclient nil)

;; JavaScript-mode config for JSON
(defun json-configuration ()
  "Set up JS-MODE with settings more common for JSON."
  (when (and buffer-file-name (string-match "\\.json\\'" buffer-file-name))
    (setq c-basic-offset 2)
    (setq js-indent-level 2)))

(add-hook 'js-mode-hook 'json-configuration)
(autoload 'js2-mode "js2-mode" nil t)
(push '("\\.js\\'" . js2-jsx-mode) auto-mode-alist)
(with-eval-after-load "js2-mode"
  (add-hook 'js2-mode-hook 'add-node-modules-path))

;; Web mode (multi-modal editing of templates)
(autoload 'web-mode "web-mode"  nil t)
(push '("\\.html\\'" . web-mode) auto-mode-alist)  ;; HTML files
(push '("\\.css\\'" . web-mode) auto-mode-alist)   ;; CSS files
(push '("\\.hbs\\'" . web-mode) auto-mode-alist)   ;; Handlebars templating
(eval-after-load "web-mode"
  '(progn
     (setq-default web-mode-enable-auto-pairing nil
                   web-mode-enable-css-colorization nil
                   web-mode-css-indent-offset 2)

     ;; If we like web-mode, we'll probably like impatient-mode too!
     (require 'impatient-mode nil)))

;;;; Mustasche mode
(autoload 'mustache-mode "mustache")
(push '("\.mustache\'" . mustache-mode) auto-mode-alist)

;;;; PHP mode
(autoload 'php-mode "php-mode")
(push '("\.php\'" . php-mode) auto-mode-alist)
(with-eval-after-load "php-mode"
  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style))
