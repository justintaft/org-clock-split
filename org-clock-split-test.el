(require 'org-clock-split)

(ert-deftest org-clock-split-get-timestrings-test ()
  (should (equal
	   (org-clock-split-get-timestrings "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24")
	   '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:44]"))))

(ert-deftest org-clock-split-split-line-test ()
  (should (equal
	   (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "20m")
	   '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:40]" "[2019-12-14 Sat 08:44]"))))

(ert-deftest org-clock-split-invalid-clock-match-test ()
  (should (equal
           (string-match org-clock-split-clock-range-regexp(concat org-clock-string ": [2019-12-14 Sat 08:20]"))
           nil)))

(ert-deftest org-clock-split-system-test ()
  (let ((expected-buffer "\
* Test
  :LOGBOOK:
CLOCK: [2019-12-18 Wed 22:02]--[2019-12-18 Wed 22:10] =>  0:08
CLOCK: [2019-12-18 Wed 22:00]--[2019-12-18 Wed 22:02] =>  0:02
  CLOCK: [2019-12-17 Tue 22:02]--[2019-12-17 Wed 22:10] =>  0:08
  CLOCK: [2019-12-17 Wed 22:00]--[2019-12-17 Tue 22:02] =>  0:02
  :END:")
        (calculated-buffer (with-temp-buffer
                             (erase-buffer)
                             (org-mode)
                             (insert "\
* Test
  :LOGBOOK:
CLOCK: [2019-12-18 Wed 22:00]--[2019-12-18 Wed 22:10] =>  0:00
  CLOCK: [2019-12-17 Wed 22:00]--[2019-12-17 Wed 22:10] =>  0:00
  :END:")
                             (outline-show-all)
                             (beginning-of-buffer)
                             (next-line)
                             (next-line)
                             (next-line)
                             (org-clock-split "2m")
                             (previous-line)
                             (org-clock-split "2m")
                             (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal expected-buffer calculated-buffer))))