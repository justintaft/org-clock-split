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

(ert-deftest org-clock-split-relative-string-to-seconds-test ()
  (should (equal
	   (org-clock-split-relative-string-to-seconds "1m")
	   60))
  (should (equal
	   (org-clock-split-relative-string-to-seconds "1h")
	   3600))
  (should (equal
	   (org-clock-split-relative-string-to-seconds "01m")
	   60))
  (should (equal
	   (org-clock-split-relative-string-to-seconds "1m1h")
	   3660))
)

(ert-deftest org-clock-split-absolute-string-to-hm-test ()
  (should (equal
	   (org-clock-split-absolute-string-to-hm "09:20")
          '(9 20))))

(ert-deftest org-clock-split-split-line-test ()
  (should (equal
          (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "4m" t)
          '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:40]" "[2019-12-14 Sat 08:44]")))
  (should (equal
          (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "20m" nil)
          '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:40]" "[2019-12-14 Sat 08:44]")))
  (should (equal
          (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "08:40" t)
          '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:40]" "[2019-12-14 Sat 08:44]")))
  (should (equal
          (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "08:40" t)
          '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 08:40]" "[2019-12-14 Sat 08:44]")))
  (should (equal
          (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "07:00" t)
          '("[2019-12-14 Sat 08:20]" "[2019-12-14 Sat 07:00]" "[2019-12-14 Sat 08:44]")))
  (should (equal
          (org-clock-split-split-line-into-timestamps "CLOCK: [2019-12-14 Sat 08:20]--[2019-12-14 Sat 08:44] =>  0:24" "07:00" nil)
          '("[2019-12-14 Sat 08:20]" "[2019-12-15 Sun 07:00]" "[2019-12-14 Sat 08:44]")))
  )

(ert-deftest org-clock-split-system-test ()
  (let ((expected-buffer "\
* Test
  :LOGBOOK:
CLOCK: [2019-12-18 Wed 22:02]--[2019-12-18 Wed 22:10] =>  0:08
CLOCK: [2019-12-18 Wed 22:00]--[2019-12-18 Wed 22:02] =>  0:02
  CLOCK: [2019-12-17 Tue 22:02]--[2019-12-17 Wed 22:10] =>  0:08
  CLOCK: [2019-12-17 Wed 22:00]--[2019-12-17 Tue 22:02] =>  0:02
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
  CLOCK: [2019-12-17 Wed 22:00]--[2019-12-17 Wed 22:10] =>  0:00
  :END:")
                             (outline-show-all)
                             (beginning-of-buffer)
                             (next-line)
                             (next-line)
                             (next-line)
                             (next-line)
                             (org-clock-split nil "2m")
                             (previous-line)
                             (org-clock-split nil "2m")
                             (previous-line)
                             (org-clock-split nil "-8m")
                             (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal expected-buffer calculated-buffer))))

(ert-deftest org-timestamp-test ()
  (should (equal
           (org-clock-split-get-clock-segment-timestamps "CLOCK: [2019-09-26 Thu 00:29]--[2019-09-26 Thu 01:11] => 0:42")
           '("2019-09-26 Thu 00:29" "2019-09-26 Thu 01:11"))))

(ert-deftest org-clock-merge-system-test ()
  (let ((expected-buffer "\
* Test
  :LOGBOOK:
  CLOCK: [2019-12-18 Wed 22:00]--[2019-12-18 Wed 22:10] =>  0:10
  CLOCK: [2019-12-17 Wed 22:00]--[2019-12-17 Wed 22:10] =>  0:10
  :END:")
        (calculated-buffer (with-temp-buffer
                             (erase-buffer)
                             (org-mode)
                             (insert "\
* Test
  :LOGBOOK:
CLOCK: [2019-12-18 Wed 22:02]--[2019-12-18 Wed 22:10] =>  0:08
CLOCK: [2019-12-18 Wed 22:00]--[2019-12-18 Wed 22:02] =>  0:02
  CLOCK: [2019-12-17 Tue 22:04]--[2019-12-17 Wed 22:10] =>  0:08
  CLOCK: [2019-12-17 Wed 22:00]--[2019-12-17 Tue 22:02] =>  0:02
  :END:")
                             (outline-show-all)
                             (beginning-of-buffer)
                             (next-line)
                             (next-line)
                             (next-line)
                             (next-line)
                             (org-clock-merge)
                             (previous-line)
                             (previous-line)
                             (org-clock-merge)
                             (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal expected-buffer calculated-buffer))))
