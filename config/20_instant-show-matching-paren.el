(setq-default show-paren-delay 0)
(setq-default show-paren-when-point-inside-paren t)
(setq-default show-paren-style 'expression)
;; This is in order for selected region to take priority over
;; show-paren expression style
(setq-default show-paren-priority -200)
(show-paren-mode +1)
