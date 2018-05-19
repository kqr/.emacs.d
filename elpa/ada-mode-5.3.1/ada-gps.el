;; Emacs ada-mode indentation engine, using GPS code in a subprocess.
;;
;; GNAT is provided by AdaCore; see http://libre.adacore.com/
;;
;;; Copyright (C) 2014, 2017  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'ada-mode)
(require 'ada-indent-user-options)

(defcustom ada-gps-size-threshold 100000
  "Max size (in characters) for using ada-wisi indentation engine.
Buffers larger than this will use ada-gps indentation engine,
which is faster on large buffers."
  :type 'integer
  :group 'ada-indentation
  :safe 'integerp)

(defvar ada-gps-debug 0)

(defconst ada-gps-indent-exec-api-version "1"
  "API version of ada_mode_gps_indent executable this code expects.")

(defvar ada-gps-indent-exec-patch-level nil
  "Patch level of ada_mode_gps_indent executable.")

;;;;; sessions

;; ada_mode_gps_indent runs a loop, waiting for indentation requests.
;;
;; We only need one process; there is no persistent state.

(cl-defstruct (ada-gps--session)
  (process nil) ;; running gpr_query
  (buffer nil))  ;; receives output of gpr_query

(defvar ada-gps-session
  (make-ada-gps--session)
  "The single instance of ada-gps--session")

(defconst ada-gps-buffer-name " *ada_gps*")

;; ada-gps-indent-exec declared in ada-mode for auto-detection of indent engine

(defun ada-gps--start-process ()
  "Start the session process running ada_gps."
  (unless (buffer-live-p (ada-gps--session-buffer ada-gps-session))
    ;; user may have killed buffer
    (setf (ada-gps--session-buffer ada-gps-session) (get-buffer-create ada-gps-buffer-name)))

  (let ((process-connection-type nil) ;; use a pipe; no need for \n or flush on Linux or Windows
	(exec-file (locate-file ada-gps-indent-exec exec-path '("" ".exe"))))
    (unless exec-file
      (error "%s not found on `exec-path'" ada-gps-indent-exec))

    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (erase-buffer); delete any previous messages, prompt
      (setf (ada-gps--session-process ada-gps-session)
	    (start-process ada-gps-buffer-name (current-buffer) exec-file))
      (set-process-query-on-exit-flag (ada-gps--session-process ada-gps-session) nil)
      (ada-gps-session-wait)

      (ada-gps-check-version)
      )))

(defun ada-gps-show-proc-id ()
  "Display ada-gps process id, for attaching with debugger."
  (interactive)
  (if (process-live-p (ada-gps--session-process ada-gps-session))
      (message "ada-gps process id: %d" (process-id (ada-gps--session-process ada-gps-session)))
    (message "ada-gps process not live")
    ))

(defun ada-gps-require-session ()
  "Create ada-gps session if not active."
  (unless (and (ada-gps--session-process ada-gps-session)
	       (process-live-p (ada-gps--session-process ada-gps-session)))
   (ada-gps--start-process)))

(defconst ada-gps-prompt "^GPS_Indent> $"
  "Regexp matching ada_mode_gps_indent prompt; indicates previous command is complete.")

(defun ada-gps-session-wait ()
  "Wait for the current command to complete."
  (unless (process-live-p (ada-gps--session-process ada-gps-session))
    (ada-gps-show-buffer)
    (error "ada-gps process died"))

  (with-current-buffer (ada-gps--session-buffer ada-gps-session)
    (let ((process (ada-gps--session-process ada-gps-session))
	  (search-start (point-min))
	  (found nil))
      (while (and (process-live-p process)
		  (progn
		    ;; process output is inserted before point, so move back over it to search it
		    (goto-char search-start)
		    (not (setq found (re-search-forward ada-gps-prompt (point-max) t)))))
	(setq search-start (point));; don't search same text again
	(accept-process-output process 0.1))
      (if found
	  (when (> ada-gps-debug 0)
	    (message "'%s'" (buffer-substring-no-properties (point-min) (point-max))))

	(ada-gps-show-buffer)
	(error "ada_gps process hung or died"))
      )))

(defun ada-gps-session-send (cmd wait prefix)
  "Send CMD to ada_gps session.
If WAIT is non-nil, wait for command to complete.
If PREFIX is non-nil, prefix with count of bytes in cmd."
  (ada-gps-require-session)
  ;; we don't wait for previous command to complete, because the
  ;; previous call to ada-gps-session-cmd might have been a partial
  ;; command string (in particular, for 'compute_indent').
  (let* ((byte-count-img (when prefix (format "%02d" (string-bytes cmd))))
	 (msg (concat byte-count-img cmd)))
    (when (and (> ada-gps-debug 0)
	       (< (length msg) 100))
      (message msg))
    (with-current-buffer (ada-gps--session-buffer ada-gps-session)
      (erase-buffer)
      (process-send-string (ada-gps--session-process ada-gps-session) msg)
      (when wait
	(ada-gps-session-wait))
      )))

(defun ada-gps-kill-session ()
  (interactive)
  (when (process-live-p (ada-gps--session-process ada-gps-session))
    (process-send-string (ada-gps--session-process ada-gps-session) "04exit")
    ))

(defun ada-gps-check-version ()
  "Throw an error if gps executable version does not match expected."
  (ada-gps-session-send "version" t t)
  (with-current-buffer (ada-gps--session-buffer ada-gps-session)
    (goto-char (point-min))
    ;; We accept any patch level
    (when (not (looking-at (concat ada-gps-indent-exec-api-version "\\....$")))
      (error "Incorrect API version for '%s'; found '%s', expecting '%s'."
	     ada-gps-indent-exec
	     (buffer-substring (point-min) (line-end-position))
	     ada-gps-indent-exec-api-version))

    (setq ada-gps-indent-exec-patch-level
	  (buffer-substring (- (line-end-position) 3) (line-end-position)))))

(defun ada-gps-show-buffer ()
  "Show ada-gps buffer."
  (interactive)
  (if (ada-gps--session-buffer ada-gps-session)
      (pop-to-buffer (ada-gps--session-buffer ada-gps-session))
    (error "ada-gps session not active")))

;;;;; indenting

(defun ada-gps-send-params ()
  "Send indentation params to current gps session."
  (ada-gps-session-send
   (format "set_params %d %d %d %d"
	   ada-indent
	   ada-indent-broken
	   ada-indent-when
	   ada-indent-record-rel-type)
   t t))

(defconst ada-gps-output-regexp " *\\([0-9]+\\) +\\([0-9]+\\)$"
  "Matches gps process output for one line.")

(defvar-local ada-gps-indent-functions nil
  "Functions to compute indentation special cases.
Called with point at current indentation of a line; return
indentation column, or nil if function does not know how to
indent that line. Run after parser indentation, so other lines
are indented correctly.")

(defun ada-gps-indent-compute ()
  "For `wisi-indent-fallback'; compute indent for current line."

  ;; always send indent parameters - we don't track what buffer we are in
  (ada-gps-send-params)

  (save-excursion
    ;; send complete current line
    (end-of-line)
    (ada-gps-session-send
     (format "compute_indent %d %d" (line-number-at-pos) (1- (position-bytes (point)))) nil t)
    (ada-gps-session-send (buffer-substring-no-properties (point-min) (point)) t nil)
    )
  (with-current-buffer (ada-gps--session-buffer ada-gps-session)
    (goto-char (point-min))
    (if (looking-at ada-gps-output-regexp)
	(string-to-number (match-string 2))

      ;; gps did not compute indent for some reason
      (when (> ada-gps-debug 0)
	(message "ada-gps returned '%s'" (buffer-substring-no-properties (point-min) (point-max))))
      0)
    ))

(defun ada-gps-indent-line ()
  "For `indent-line-function'; indent current line using the ada-gps indentation engine."
  (let ((savep (copy-marker (point)))
	(to-indent nil))
    (back-to-indentation)
    (when (>= (point) savep)
      (setq to-indent t))

    (ada-gps-indent-region (line-beginning-position) (line-end-position))

    (goto-char savep)
    (when to-indent (back-to-indentation))
    ))

(defun ada-gps-indent-region (begin end)
  "For `indent-region-function'; indent lines in region BEGIN END using GPS."

  (save-excursion
    ;; always send indent parameters - we don't track what buffer we are in
    (ada-gps-send-params)

    ;; send complete lines
    (goto-char end)
    (setq end (line-end-position))

    (let ((source-buffer (current-buffer))
	  (begin-line (line-number-at-pos begin))
	  (end-line (line-number-at-pos end))
	  (delete-bogus nil)
	  (failed nil))

      (goto-char begin)
      (when (and (= begin-line end-line)
		 (eolp))
	;; Indenting a single blank line. Insert some text so the GPS
	;; engine won't return 0.
	;;
	;; test/ada-gps/ada_gps_bug_005.adb
	(insert "bogus")
	(setq end (point))
	(setq delete-bogus t))

      (ada-gps-session-send
       (format "compute_region_indent %d %d %d" begin-line end-line (1- (position-bytes end))) nil t)
      (ada-gps-session-send (buffer-substring-no-properties (point-min) end) t nil)

      (when delete-bogus
	(backward-delete-char 5))

      (with-current-buffer (ada-gps--session-buffer ada-gps-session)
	;; buffer contains two numbers per line; Emacs line number,
	;; indent. Or an error message.
	(goto-char (point-min))
	(while (and (not failed)
		    (not (looking-at ada-gps-prompt)))
	  (if (looking-at ada-gps-output-regexp)
	      (let ((line (string-to-number (match-string 1)))
		    (indent (string-to-number (match-string 2))))
		(with-current-buffer source-buffer
		  (goto-char (point-min))
		  (forward-line (1- line)) ;; FIXME: count forward from prev indented line
		  (indent-line-to indent)
		  )

		(forward-line 1))

	    ;; else some error message
	    (setq failed t)
	    (cond
	     ((> ada-gps-debug 0)
	      (message "ada-gps returned '%s'" (buffer-substring-no-properties (point-min) (point-max)))
	      (goto-char (point-max)))
	     (t
	      (message "ada-gps indent failed")
	      )))
	  ))

      (unless failed
	;; run ada-gps-indent-functions on region
	(goto-char begin)
	(let ((line begin-line)
	      indent)
	  (while (<= line end-line)
	    (back-to-indentation)
	    (setq indent
		  (run-hook-with-args-until-success 'ada-gps-indent-functions))
	    (when indent
	      (indent-line-to indent))
	    (forward-line 1)
	    (setq line (1+ line))
	    )))
      )))

(defun ada-gps--comment-goto-start()
  ;; If comment is after a terminal semicolon, indent to
  ;; beginning of statement, not prev line.
  ;;
  ;; test/ada-gps/ada_gps_bug_007.adb
  (let (cache)
    (end-of-line)
    (wisi-validate-cache (point))
    (wisi-backward-token)
    (setq cache (wisi-get-cache (point)))
    (when (and cache
	       (eq 'statement-end (wisi-cache-class cache)))
      (wisi-goto-start cache))))

(defun ada-gps-comment ()
  "Modify indentation of a comment:
For `ada-gps-indent-functions'.
- align to previous comment after code.
- align to previous code
- respect `ada-indent-comment-gnat'."
  ;; We know we are at the first token on a line. We check for comment
  ;; syntax, not comment-start, to accomodate gnatprep, skeleton
  ;; placeholders, etc.
  ;;
  ;; The ada-gps indentation algorithm has already indented the
  ;; comment; however, it gets this wrong in many cases, so we don't
  ;; trust it. We compare the ada-gps indent to previous and following
  ;; code indent, then choose the best fit.
  (when (and (not (eobp))
	     (= 11 (syntax-class (syntax-after (point)))))

    ;; We are looking at a comment; check for preceding comments, code
    (let ((indent (current-column))
	  after prev-indent next-indent)

      (if (save-excursion (forward-line -1) (looking-at "\\s *$"))
	  ;; after blank line - find a code line
	  (save-excursion
	    (setq after 'code)
	    (forward-line -1)
	    (while (or (bobp)
		       (looking-at "\\s *$"))
	      (forward-line -1))
	    (if (bobp)
		(setq prev-indent 0)

	      (ada-gps--comment-goto-start)
	      (back-to-indentation)
	      (setq prev-indent (current-column)))
	    )

	;; after a code line
	(save-excursion
	  (forward-comment -1)
	  (if (eolp)
	      ;; no comment on previous line
	      (progn
		(setq after 'code)
		(ada-gps--comment-goto-start)
		(back-to-indentation)
		(setq prev-indent (current-column)))

	    (setq prev-indent (current-column))
	    (if (not (= prev-indent (progn (back-to-indentation) (current-column))))
		;; previous line has comment following code
		(setq after 'code-comment)
	      ;; previous line has plain comment
	      (setq after 'comment)
	      )))
	)

      (cl-ecase after
	(code
	 (if ada-indent-comment-gnat
	     (ada-wisi-comment-gnat indent 'code)

	   ;; Find indent of following code
	   (save-excursion
	     (forward-line 1)
	     (while (or (eobp)
			(looking-at "\\s *$"))
	       (forward-line 1))
	     (if (eobp)
		 (setq next-indent 0)
	       (back-to-indentation)
	       (setq next-indent (current-column))))

	   (cond
	    ((or (= indent prev-indent)
		 (= indent next-indent))
	     indent)
	    (t
	     prev-indent))))

	(comment
	 prev-indent)

	(code-comment
	 (if ada-indent-comment-gnat
	     (ada-wisi-comment-gnat indent 'code-comment)

	   ;; After comment that follows code on the same line
	   ;; test/ada_mode-nominal.adb
	   ;;
	   ;; begin -- 2
	   ;;       --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at "Bad_Thing"))
	   prev-indent)
	 ))
      )))

;;;;; setup

(defun ada-gps-setup ()
  "Set up a buffer for indenting with ada-gps."
  (set (make-local-variable 'indent-line-function) 'ada-gps-indent-line)
  (set (make-local-variable 'indent-region-function) 'ada-gps-indent-region)
  (ada-gps-require-session)

  ;; file local variables may have added opentoken, gnatprep
  (setq ada-gps-indent-functions (append ada-gps-indent-functions (list #'ada-gps-comment)))
  )

(require 'ada-wisi)

(defun ada-gps-or-wisi-setup ()
  "If buffer size > `ada-gps-size-threshold', use ada-gps;
otherwise use ada-wisi indentation engine with ada-gps fallback,"
  ;; ada-gps-size-threshold can be set in file-local variables, which
  ;; are parsed after ada-mode-hook runs.
  (add-hook 'hack-local-variables-hook 'ada-gps-post-local-vars nil t))

(defun ada-gps-post-local-vars ()
  "See `ada-gsp-or-wisi-setup'"
  (setq hack-local-variables-hook (delq 'ada-gps-post-local-vars hack-local-variables-hook))

  (if (> (point-max) ada-gps-size-threshold)
      (progn
	;; use ada-gps for indent, ada-wisi for face, navigation
	(ada-wisi-setup)
	(ada-gps-setup))

    (ada-wisi-setup)
    (set (make-local-variable 'indent-region-function) nil)
    (setq wisi-indent-fallback 'ada-gps-indent-compute)
    ))

(provide 'ada-gps)

(unless (locate-file ada-gps-indent-exec exec-path '("" ".exe"))
  (error "%s not found on `exec-path'" ada-gps-indent-exec))

(add-hook 'ada-mode-hook 'ada-gps-or-wisi-setup)
(setq ada-mode-hook (delq 'ada-wisi-setup ada-mode-hook))

;; end of file
