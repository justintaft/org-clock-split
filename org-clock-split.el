;;; org-clock-split.el --- Split clock entries -*- lexical-binding: t; -*-

;; Author: Justin Taft <https://github.com/justintaft>
;; Keywords: calendar
;; URL: https://github.com/justintaft/org-clock-split
;; Version: 1.0
;; Package-Requires: ((emacs "24"))

;;; Contributors
;;  https://github.com/swflint
;;  https://github.com/alphapapa
;;  https://github.com/miguelmorin

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

(defun org-clock-split-split-time-string-to-minutes (time-string)
  (interactive)
  "Return minutes given a time string in format.
Throws error when invalid time string is given.
   TIME-STRING - Time offset to split record at.  (Ex '1h', '01m', '68m1h', '1:05')"
  
  ;; Remove all whitespace from string for sanity checks.
  ;; Used to ensure all characters are processed.
  (if (string-match "[ \t]+" time-string)
      (setq time-string (replace-match  "" t t time-string)))

  (if (string-match ":" time-string)
      (progn
	(setq time-array (split-string time-string ":"))
	(if (/= 2 (length time-array))
	    (error "Invalid time string format. Can you write 0:05 for 0h5m, for example?")
	  )
	(+ (* 60 (string-to-number (car time-array))) (string-to-number (car (cdr time-array))))
	)
    (let ((total-minutes 0)
          (matched-input-characters 0))
      
      (when (string-match "\\([0-9]+\\)h" time-string)
	(cl-incf total-minutes (* 60 (string-to-number (match-string 1 time-string))))
	(cl-incf matched-input-characters (+ 1 (length (match-string 1 time-string)))))
      
      (when (string-match "\\([0-9]+\\)m" time-string)
	(cl-incf total-minutes (string-to-number (match-string 1 time-string)))
	(cl-incf matched-input-characters (+ 1 (length (match-string 1 time-string)))))
      
      (if (/= matched-input-characters (length time-string))
          (error "Invalid time string format"))

      total-minutes))
  )

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

  (let ((parsed-minutes (org-clock-split-split-time-string-to-minutes time-string))
        original-line clockin-text clockout-text temp-position)
    
    ;; Copy line
    (move-beginning-of-line nil)
    (setq original-line (buffer-substring (line-beginning-position) (line-beginning-position 2)))

    ;; Error if CLOCK line does not contain check in and check out time
    (if (not (string-match  org-ts-regexp-both  original-line))
        (error "Cursor must be placed on line with valid CLOCK entry"))

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
