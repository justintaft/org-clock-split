;;; org-clock-split.el --- Split clock entries -*- lexical-binding: t; -*-

;; Author: Justin Taft <https://github.com/justintaft>
;; Keywords: calendar
;; URL: https://github.com/justintaft/emacs-org-clock-split
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;;; Contributors
;;  https://github.com/swflint
;;  https://github.com/alphapapa

;;; Commentary:
;;
;;  This package provides ability to split an org CLOCK entry into two records.
;;
;;  Usage example:
;;
;;  If cursor is on
;;
;;  CLOCK: [2018-08-30 Thu 12:19]--[2018-08-30 Thu 16:05] =>  3:46
;;  
;;  Running
;;
;;  (org-clock-split \"1h2m\")
;;  
;;  Will produce
;;
;;  CLOCK: [2018-08-30 Thu 12:19]--[2018-08-30 Thu 13:21] =>  1:02
;;  CLOCK: [2018-08-30 Thu 13:21]--[2018-08-30 Thu 16:05] =>  2:44"

;;; Code:
(require 'cl-lib)
(require 'org)

(defvar org-clock-split-inactive-timestamp-hm (replace-regexp-in-string "<" "[" (replace-regexp-in-string ">" "]" (cdr org-time-stamp-formats)))
  "Inactive timestamp with hours and minutes. I don't know where org mode provides it, or why it doesn't.")

(defvar org-clock-split-clock-range-regexp (concat "^" org-clock-string " " org-tr-regexp-both)
  "Regular expression to match a clock range, possibly without the interval calculation at the end ('=> hh:mm').")

(defun org-clock-split-splitter-string-to-minutes (splitter-string)
  "Return minutes given a time string in format.
Throws error when invalid time string is given.
   SPLITTER-STRING - Time offset to split record at.  (Ex '1h', '01m', '68m1h')"
  
  ;; Remove all whitespace from string for sanity checks.
  ;; Used to ensure all characters are processed.
  (if (string-match "[ \t]+" splitter-string)
      (setq splitter-string (replace-match  "" t t splitter-string)))

  (let ((total-minutes 0)
        (matched-input-characters 0))

    (when (string-match "\\([0-9]+\\)h" splitter-string)
      (cl-incf total-minutes (* 60 (string-to-number (match-string 1 splitter-string))))
      (cl-incf matched-input-characters (+ 1 (length (match-string 1 splitter-string)))))

    (when (string-match "\\([0-9]+\\)m" splitter-string)
      (cl-incf total-minutes (string-to-number (match-string 1 splitter-string)))
      (cl-incf matched-input-characters (+ 1 (length (match-string 1 splitter-string)))))
    
    (if (/= matched-input-characters (length splitter-string))
        (error "Invalid time string format"))

    total-minutes))

(defun org-clock-split-get-timestrings (tr-string)
  "Gets the clock-in and clock-out timestrings from a time range string."
  (let* ((t1-start (string-match org-ts-regexp-both tr-string 0))
	 (t1-end (match-end 0))
	 (t2-start (string-match org-ts-regexp-both tr-string t1-end))
	 (t2-end (match-end 0))
	 (t1 (substring tr-string t1-start t1-end))
	 (t2 (substring tr-string t2-start t2-end)))
    (list t1 t2)))

(ert-deftest org-clock-split-get-timestrings-test ()
  (should (equal
	   (org-clock-split-get-timestrings "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24")
	   '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:44]"))))

(defun org-clock-split-split-line-into-timestamps (original-line splitter-string)
  "Splits the clock range in original-line by splitter-string, currently a duration segment such as 1h02m."
  (let* ((parsed-minutes (org-clock-split-splitter-string-to-minutes splitter-string))
	 (timestring-pair (org-clock-split-get-timestrings original-line))
	 (t0string (pop timestring-pair))
	 (t2string (pop timestring-pair))
	 (t0 (float-time (apply #'encode-time (org-parse-time-string t0string))))
	 (t1 (+ t0 (* 60 parsed-minutes)))
	 (t1string (format-time-string org-clock-split-inactive-timestamp-hm t1)))
    (list t0string t1string t2string)))

(ert-deftest org-clock-split-split-line-test ()
  (should (equal
	   (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "20m")
	   '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:40]" "[2019-12-14 Sat 08:44]"))))


(defun org-clock-split-get-next-time-string ()
  "Gets next time string in CLOCK entry in buffer relative to cursor position."
  (let (time-string first-position)
    (re-search-forward "\\[")
    (backward-char)
    (setq first-position (point))
    (re-search-forward "\\]")
    (setq time-string (buffer-substring first-position (point)))
    time-string))

(defun org-clock-split  (time-string)
  "Split CLOCK entry under cursor into two entries.
Total time of created entries will be the same as original entry.

   WARNING: Negative time entries can be created if splitting at an offset
longer then the CLOCK entry's total time.

   TIME-STRING: Time offset to split record at.  Examples: '1h', '01m', '68m1h'."

  (interactive "sTime offset to split clock entry (ex 1h2m): ")

  (let ((parsed-minutes (org-clock-split-splitter-string-to-minutes time-string))
        original-line clockin-text clockout-text temp-position)
    
    ;; Copy line
    (move-beginning-of-line nil)
    (setq original-line (buffer-substring (line-beginning-position) (line-beginning-position 2)))

    ;; Error if CLOCK line does not contain check in and check out time
    (unless (string-match org-clock-split-clock-range-regexp original-line)
      (error "Cursor must be placed on line with valid CLOCK entry range"))

    (move-end-of-line nil)
    (newline)
    (insert original-line)
    (delete-char 1)
    
    ;; Move to previous line
    (previous-line)
    (previous-line)
    
    ;; Copy start time to end time
    (setq clockin-text (org-clock-split-get-next-time-string))

    (re-search-forward "--")
    (kill-line)
    (insert clockin-text)
    
    ;; Update timestamp with parsed minutes
    (org-timestamp-change parsed-minutes  'minute)
    
    ;; Create copy of created end time, as new record
    ;; will start at this time.
    (re-search-backward "]-")
    (setq clockout-text (org-clock-split-get-next-time-string))
    
    (forward-line 1)
    (move-beginning-of-line nil)

    (re-search-forward "\\[")
    (backward-char)
    (setq tmp-position (point))
    (re-search-forward "\\]")
    (delete-region tmp-position (point))

    (insert clockout-text)
    
    ;; Update timestamp to reflect new value
    (org-ctrl-c-ctrl-c)))

(provide 'org-clock-split)

;;; org-clock-split.el ends here
