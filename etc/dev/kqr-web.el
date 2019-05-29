;;;; REST client mode
(require 'restclient nil)


(autoload 'js2-mode "js2-mode" nil t)
(push '("\\.js\\'" . js2-jsx-mode) auto-mode-alist)
(push '("\\.jsx\\'" . js2-jsx-mode) auto-mode-alist)
(with-eval-after-load "js2-mode"
  (defun javascript-configuration ()
    "Configure two spaces indent for JavaScript."
    (setq js2-basic-offset 2)
    (setq js-indent-level js2-basic-offset)
    (setq c-basic-offset js-indent-level))

  (add-hook 'js2-mode-hook 'javascript-configuration)
  (add-hook 'js2-mode-hook 'add-node-modules-path))

(autoload 'css-mode "css-mode" nil t)
(push '("\\.css\\'" . css-mode) auto-mode-alist)

;;;; Mustasche mode
(autoload 'mustache-mode "mustache")
(push '("\.mustache\'" . mustache-mode) auto-mode-alist)

;; Web mode (multi-modal editing of templates)
(autoload 'web-mode "web-mode"  nil t)
(push '("\\.html\\'" . web-mode) auto-mode-alist)
(push '("\\.hbs\\'" . web-mode) auto-mode-alist)  ;; Handlebars templating
(eval-after-load "web-mode"
  '(progn
     (setq-default web-mode-enable-auto-pairing nil
                   web-mode-enable-css-colorization nil
                   web-mode-css-indent-offset 2)

     ;; If we like web-mode, we'll probably like impatient-mode too!
     (require 'impatient-mode nil)))

;;;; PHP mode
(autoload 'php-mode "php-mode")
(push '("\.php\'" . php-mode) auto-mode-alist)
(with-eval-after-load "php-mode"
  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style))
