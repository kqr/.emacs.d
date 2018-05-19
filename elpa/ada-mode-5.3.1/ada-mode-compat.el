;; ada-mode-compat.el --- Implement current Emacs features not present earlier versions  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(when (not (functionp 'font-lock-ensure))
  ;; not in 24.3, 24.4, 24.5; in 25.1
  (defun font-lock-ensure (&optional beg end)
    (font-lock-fontify-region (or beg (point-min)) (or end (point-max)))))

(provide 'ada-mode-compat)

;; end of file
