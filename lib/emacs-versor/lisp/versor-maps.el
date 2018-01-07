;;; versor-maps.el --- separate module to avoid recursive dependencies
;;; Time-stamp: <2018-01-07 18:09:11 kqr>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;; Code:

(defvar versor-insertion-placement-keymap (make-sparse-keymap "Versor insert place")
  "Keymap for reading what place to do insertion at.
This is for choosing before, after, around or inside.")

(fset 'versor-insertion-placement-keymap versor-insertion-placement-keymap)

(defvar versor-altering-mode-map (make-sparse-keymap "Versor alter item")
  "Keymap for altering the selected item.")

(fset 'versor-altering-mode-map versor-altering-mode-map)

(defvar versor-original-bindings-map
  (make-sparse-keymap "Versor non-versor bindings")
  "A keymap holding the key bindings that Versor displaced.")

(defun versor-global-set-key (key command)
  "Like `global-set-key', for KEY and COMMAND.
If the old binding was not a versor one, save the old binding."
  (let ((old-value (lookup-key (current-global-map) key t)))
    (when (and (symbolp old-value)
	       (not (string-match "^versor-" (symbol-name old-value))))
      (message "Versor changing binding of %s from %s to %s"
	       (key-description key)
	       (symbol-name old-value)
	       (symbol-name command))
      (define-key versor-original-bindings-map key old-value)))
  (global-set-key key command))

(provide 'versor-maps)
;;; versor-maps.el ends here
