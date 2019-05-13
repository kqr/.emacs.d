(when (require 'ps-print nil)
  (add-to-list
   'ps-font-info-database
   '(kqr-mixed-family (fonts (normal . "Helvetica")
                             (bold . "Helvetica-Bold")
                             (italic . "Helvetica-Oblique")
                             (bold-italic . "Helvetica-BoldOblique")
                             (org-verse . "Courier")
                             (fixed-pitch . "Courier"))
                      (size . 10.0)
                      (line-height . 11.56)
                      (space-width . 2.78)
                      (avg-char-width . 5.09243)))
  (setq ps-print-header t
        ps-header-lines 1
        ps-font-family 'kqr-mixed-family
        ps-font-size '(10 . 10)
        ps-line-spacing 0
        ps-paper-type 'a4)
  (setq ps-number-of-columns 1
        ps-left-margin 60
        ps-inter-column 20
        ps-right-margin 70)
  (setq ps-top-margin 20
        ps-header-pad 0
        ps-header-offset 10
        ps-footer-offset 20
        ps-bottom-margin 40)
  (setq ps-print-header-frame t
        ps-print-only-one-header t
        ps-header-font-family 'kqr-mixed-family
        ps-header-title-font-size '(10 . 10)
        ps-header-font-size '(10 . 10)
        ps-header-frame-alist
        '((fore-color . 0.0)
          (back-color . 1.0)
          (shadow-color . 1.0)
          (border-color . 0.0)
          (border-width . 0.4))))
