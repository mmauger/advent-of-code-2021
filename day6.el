;;; day6.el --- Advent of Code 2021 Lanternfish      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Michael R. Mauger

;; Author: Michael R. Mauger <michael@mauger.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Lanternfish reproduction

;;; Code:

(defvar day6-test '(3 4 3 1 2))

(defvar day6-data '(4 3 4 5 2 1 1 5 5 3 3 1 5 1 4 2 2 3 1 5 1 4 1
2 3 4 1 4 1 5 2 1 1 3 3 5 1 1 1 1 4 5 1 2 1 2 1 1 1 5 3 3 1 1 1 1
2 4 2 1 2 3 2 5 3 5 3 1 5 4 5 4 4 4 1 1 2 1 3 1 1 4 2 1 2 1 2 5 4
2 4 2 2 4 2 2 5 1 2 1 2 1 4 4 4 3 2 1 2 4 3 5 1 1 3 4 2 3 3 5 3 1
4 1 1 1 1 2 3 2 1 1 5 5 1 5 2 1 4 4 4 3 2 2 1 2 1 5 1 4 4 1 1 4 1
4 2 4 3 1 4 1 4 2 1 5 1 1 1 3 2 4 1 1 4 1 4 3 1 5 3 3 3 4 1 1 3 1
3 4 1 4 5 1 4 1 2 2 1 3 3 5 3 2 5 1 1 5 1 5 1 4 4 3 1 5 5 2 2 4 1
1 2 1 2 1 4 3 5 5 2 3 4 1 4 2 4 4 1 4 1 1 4 2 4 1 2 1 1 1 1 1 1 3
1 3 3 1 1 1 1 3 2 3 5 4 2 4 3 1 5 3 1 1 1 2 1 4 4 5 1 5 1 1 1 2 2
4 1 4 5 2 4 5 2 2 2 5 4 4))

(defun day6a-one-day (school)
  "Age the SCHOOL one day and add new fish."
  (let (new-school
        (new-fish 0))
    (mapc
     (lambda (f)
       (if (not (zerop f))
           (push (1- f) new-school)
         (push 6 new-school)
         (cl-incf new-fish)))
     school)
    (dotimes (_f new-fish)
      (push 8 new-school))
    (nreverse new-school)))

(defun day6a-80-days (init &optional days)
  "Age school over DAYS days starting at INIT."
  (let ((school (mapcar #'1+ init))
        population)
    (dotimes (day (1+ (or days 80)))
      (setq school (day6a-one-day school))
      (push (length school) population)
      (message "After %2d days: %S" day school)
      )
    (list population school)))

(day6a-80-days day6-test 18)
;; => ((26 22 21 20 20 19 17 15 12 11 10 10 ...) (6 0 6 4 5 6 0 1 1 2 6 0 ...))
(day6a-80-days day6-test 80)
;; => ((5934 5363 4993 4573 4181 3862 3464 3225 2894 2709 2469 2284 ...) (0 1 0 5 6 0 1 2 2 3 0 1 ...))
(day6a-80-days day6-data 80)
;; => ((359344 323335 301836 274827 252638 232817 209768 195372 174946 163972 148389 137864 ...) (1 0 1 2 6 5 5 2 2 0 0 5 ...))

;; Rather than a cell per fish with its place in the cycle, this
;; version creates a vector indexed by the place in the cycle and
;; stores a count of fish in that place

(defun day6b-one-day (school)
  "Age the SCHOOL one day and add new fish."
  (let ((new-school (make-vector 9 0))
        (ready-spawn (aref school 0)))
    (dotimes (d (length school))
      (if (> d 0)
          (cl-incf (aref new-school (1- d)) (aref school d))
        (cl-incf (aref new-school 8) (aref school d)) ; new spawn
        (cl-incf (aref new-school 6) (aref school d)))) ; reset for next cycle
    new-school))

(defun day6b-80-days (init &optional days)
  "Age school over DAYS days starting at INIT."
  (let ((school (make-vector 9 0))
        population)
    (message "[0]=%S" school ) ;; (aref school 0))
    (dolist (f (mapcar #'1+ init))
      (cl-incf (aref school f)))
    (message "%S" school)
    (dotimes (day (1+ (or days 80)))
      (setq school (day6b-one-day school))
      (push (apply #'+ (append school nil)) population)
      (message "After %2d days: %S" day school)
      )
    (list population school)))

(day6b-80-days day6-test 18)
;; => ((26 22 21 20 20 19 17 15 12 11 10 10 ...) [3 5 3 2 2 1 5 1 4])
(day6b-80-days day6-test 80)
;; => ((5934 5363 4993 4573 4181 3862 3464 3225 2894 2709 2469 2284 ...) [424 729 558 790 739 762 991 370 571])
(day6b-80-days day6-test 256)
;; => ((26984457539 24654746147 22669256596 20723155359 19026658353 17430929182 15963109809 14661544436 13398199926 12322913103 11256546221 10346343493 ...) [2376852196 2731163883 2897294544 3164316379 3541830408 3681986557 4275812629 1985489551 2329711392])
(day6b-80-days day6-data 256)
;; => ((1629570219571 1488952700013 1369425031012 1251150428444 1149615965856 1052289952509 964470457177 885260659343 809276967569 744309560228 679675732444 625115470784 ...) [144177205175 163803187106 176535811181 189353957920 215600615915 221062131589 258892122126 119527669001 140617519558])

(provide 'day6)
;;; day6.el ends here
