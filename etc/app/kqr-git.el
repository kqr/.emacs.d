(autoload 'magit-status "magit")
(define-key global-map (kbd "<f3>") 'magit-status)
(with-eval-after-load "magit"
  ;; Enable all subcommands and options everywhere
  (setq transient-default-level 7)

  (setq magit-log-margin '(t age magit-log-margin-width t 10))
  (setq magit-diff-refine-hunks 'all)
  ;; I used to try to set magit-log-arguments and magit-diff-arguments here,
  ;; but I have since learned that those are set in etc/transient/values.el!
  ;; And I pretty much always want to use those unless otherwise specified.
  (setq magit-use-sticky-arguments nil)

  ;; Restore visibility of foldable sections when opening again
  (setq magit-section-cache-visibility nil)

  ;; Backup uncommitted WIP changes to prevent serious foot-shooting
  (magit-wip-mode +1)

  (setq magit-no-confirm '(safe-with-wip)))
