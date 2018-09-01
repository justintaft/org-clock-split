(defun jt/org-split-time-string-to-minutes (time-string)
  "Return minutes given a time string in format. 
   Throws error when invalid time string is given.
   Example strings: 1h, 01m, 1h2m, 68m1h"
  

  ;remove all whitespace from string
  (if (string-match "[ \t]+" time-string)
    (setq time-string (replace-match  "" t t time-string)))

  (let ((total-minutes 0)
        (matched-input-characters 0))

    (when (string-match "\\([0-9]+\\)h" time-string)
      (cl-incf total-minutes (* 60 (string-to-number (match-string 1 time-string))))
      (cl-incf matched-input-characters (+ 1 (length (match-string 1 time-string)))))

    (when (string-match "\\([0-9]+\\)m" time-string)
      (cl-incf total-minutes (string-to-number (match-string 1 time-string)))
      (cl-incf matched-input-characters (+ 1 (length (match-string 1 time-string)))))
        
    (if (/= matched-input-characters (length time-string))
      (error "Invalid time string format.")) 

    total-minutes))

(defun jt/org-get-next-time-string ()
    (let ((time-string ""))
      (re-search-forward "\\[")
      (backward-char)
      (push-mark)
      (re-search-forward "\\]")
      (activate-mark)
      (setq time-string (buffer-substring (mark) (point)))
      (deactivate-mark)
      time-string))

(defun jt/org-split-time (time-string)
  "Split CLOCK entry at offset in to two entries. 
   Total time of created entries will be the same as original entry. 

   WARNING: Negative time entries can be created if splitting at an offset longer
   then the CLOCK entry's total time. 

   Ex: If cursor is on 

   CLOCK: [2018-08-30 Thu 12:19]--[2018-08-30 Thu 16:05] =>  3:46
   
   Running
   (jt/org-split-time \"1h2m\") 
   
   Will produce

   CLOCK: [2018-08-30 Thu 12:19]--[2018-08-30 Thu 13:21] =>  1:02
   CLOCK: [2018-08-30 Thu 13:21]--[2018-08-30 Thu 16:05] =>  2:44"

   

  (interactive "sTime offset to split clock entry (ex 1h2m): ")

  (let ((original-line "")
        (clockin-text "")
        (clockout-text "")
        (parsed-minutes (jt/org-split-time-string-to-minutes time-string)))
  
    (move-beginning-of-line nil)
    

    ;Copy line
    (setq original-line (buffer-substring (line-beginning-position) (line-beginning-position 2)))

    ;Error if CLOCK line does not contain check in and check out time
    (if (not (string-match  org-ts-regexp-both  original-line))
      (error "Cursor must be placed on line with valid CLOCK entry."))

    
    (move-end-of-line nil)
    (newline)
    (insert original-line)
    (delete-char 1)
    
    ;Move to previous line
    (previous-line)
    (previous-line)
     
    
    ;Copy start time to end time
    (setq clockin-text (jt/org-get-next-time-string))

    (re-search-forward "--")
    (kill-line)
    (insert clockin-text)
    
    ;Navigate to colon and subtract 
    ;modify timestamp
    (org-timestamp-change parsed-minutes  'minute )
    
    ;Grab final time
    (re-search-backward "]-")
    (setq clockout-text (jt/org-get-next-time-string))
    
    (forward-line 1)
    (move-beginning-of-line nil)

    (re-search-forward "\\[")
    (backward-char)
    (push-mark)
    (re-search-forward "\\]") 
    (activate-mark)
    (delete-region (mark) (point))
    (deactivate-mark)


    (insert clockout-text)
    
    ;update timestamp to reflect new value
    (org-ctrl-c-ctrl-c)

))
