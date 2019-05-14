;; Meghanada (NOT jdee!) for Java development
;; Should automatically install meghanada-server?
(add-hook
 'java-mode-hook
 '(lambda ()
    (require 'meghanada)
    (meghanada-mode +1)
    (setq c-basic-offset 2)
    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")
    (define-key global-map (kbd "C-c C-v C-b") 'meghanada-compile-project)
    (define-key global-map (kbd "C-c C-v C-r") 'meghanada-exec-main)))

;; Scala mode & sbt mode
(autoload 'scala-mode "scala-mode")
(push '("\\.scala\\'" . scala-mode) auto-mode-alist)
(push '("\\.sbt\\'" . scala-mode) auto-mode-alist)
(with-eval-after-load "scala-mode"
  (setq sbt:prefer-nested-projects t))