;;; day2.el --- Advent of code day 2                 -*- lexical-binding: t; -*-

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

;;

;;; Code:

(defvar day2-instruct '(
forward 4
down 8
down 1
forward 6
forward 7
down 7
forward 3
forward 5
up 9
down 1
forward 5
down 8
forward 4
forward 5
down 5
down 1
forward 1
down 3
forward 5
forward 5
down 1
up 2
down 2
down 5
down 5
forward 3
forward 7
forward 5
forward 9
forward 8
down 4
down 6
up 5
down 1
forward 6
up 3
forward 7
forward 4
down 7
up 5
up 5
up 1
up 5
forward 5
forward 2
forward 7
down 7
forward 9
down 9
up 8
up 8
up 2
forward 5
forward 8
up 5
forward 1
down 1
down 6
forward 1
forward 2
forward 4
forward 6
up 4
up 5
down 4
down 9
down 4
forward 4
up 8
up 2
down 2
up 9
forward 9
forward 4
forward 1
forward 6
up 3
forward 6
forward 2
up 3
down 3
forward 6
down 9
down 7
forward 3
up 7
up 8
forward 3
down 1
down 8
forward 7
forward 3
down 2
down 5
forward 5
forward 1
down 1
down 3
down 5
forward 1
down 1
down 7
forward 1
up 2
down 5
up 3
up 2
down 7
up 4
forward 2
down 3
down 1
up 7
down 6
down 1
forward 7
down 5
down 2
forward 7
up 9
forward 6
forward 6
forward 2
forward 6
down 2
forward 4
down 5
forward 4
down 8
forward 3
down 9
up 5
forward 6
down 5
forward 5
down 4
down 1
forward 3
up 9
up 5
up 9
down 3
forward 7
forward 7
up 5
up 6
up 3
down 9
down 4
up 8
down 9
down 6
forward 5
down 6
forward 7
down 4
down 9
down 9
forward 6
down 4
up 2
down 8
up 3
up 7
up 1
forward 9
down 4
down 8
up 2
forward 7
forward 5
down 9
down 9
up 5
down 4
forward 8
up 3
up 4
up 8
down 7
forward 6
down 8
down 1
up 1
down 7
down 7
forward 3
down 9
up 2
forward 2
up 1
up 1
down 2
down 8
up 5
down 3
down 3
forward 2
down 4
forward 2
down 2
forward 3
down 6
forward 8
down 5
down 6
forward 9
forward 2
down 6
down 4
up 9
forward 2
forward 1
up 9
down 9
forward 8
down 4
up 3
down 1
forward 9
forward 9
forward 3
forward 4
down 2
down 1
forward 5
up 3
forward 6
down 8
down 8
down 7
forward 1
forward 6
down 9
down 6
forward 8
down 5
up 6
down 2
forward 2
up 3
forward 6
forward 4
up 4
down 5
forward 2
down 5
forward 1
forward 5
up 7
up 1
down 3
up 8
forward 4
forward 8
forward 8
up 2
down 8
up 2
up 2
up 7
down 9
down 1
forward 1
down 3
down 1
down 4
forward 3
down 4
down 5
forward 7
forward 6
forward 7
forward 8
up 6
down 1
down 9
up 2
up 2
forward 1
up 9
forward 6
down 2
forward 6
forward 8
up 8
down 6
forward 2
up 4
up 5
down 3
down 2
forward 7
down 8
forward 4
forward 8
up 4
down 7
forward 6
forward 1
up 4
down 4
down 9
down 7
down 6
down 1
forward 7
up 3
down 1
down 9
down 9
down 1
down 7
down 8
up 9
down 7
up 4
forward 4
down 2
up 8
down 6
down 6
forward 4
up 5
down 9
down 8
up 7
down 4
forward 9
up 3
down 6
forward 7
up 4
forward 9
down 6
forward 6
down 3
down 5
down 4
up 5
down 8
down 8
forward 5
forward 1
down 3
forward 7
down 3
up 6
forward 5
up 7
forward 8
down 1
forward 7
forward 8
forward 9
forward 7
up 5
forward 9
up 7
down 7
forward 8
down 8
up 6
down 4
forward 6
forward 3
forward 3
forward 6
down 3
up 4
down 3
down 8
forward 2
down 1
down 5
forward 2
up 3
up 5
forward 2
forward 8
down 7
down 9
forward 8
forward 5
forward 2
down 3
forward 6
forward 3
forward 4
forward 9
down 8
forward 2
down 6
down 8
forward 1
forward 5
up 3
forward 8
up 3
forward 2
down 3
down 5
up 4
down 9
up 5
down 2
forward 7
forward 8
forward 2
forward 4
forward 6
down 1
up 3
forward 3
up 6
forward 1
down 9
forward 4
forward 5
forward 3
down 7
down 9
forward 1
forward 5
up 1
down 6
down 7
up 4
up 7
forward 2
down 7
forward 5
up 9
up 8
forward 8
up 1
up 6
down 7
up 8
forward 2
down 1
forward 7
forward 6
forward 2
up 7
down 5
down 6
forward 8
down 3
down 2
forward 5
down 7
forward 2
down 9
forward 7
forward 9
forward 1
down 7
down 3
down 8
down 4
up 1
down 2
forward 5
forward 9
forward 5
up 6
up 1
forward 3
forward 1
forward 7
down 9
forward 4
down 7
up 6
forward 1
down 7
forward 5
down 4
down 2
up 1
forward 6
up 6
down 3
up 5
down 8
down 5
forward 2
down 1
forward 8
forward 4
down 3
forward 3
forward 6
forward 2
forward 9
forward 2
down 3
forward 8
down 4
down 1
forward 4
down 1
forward 5
down 5
down 6
forward 6
down 6
down 9
forward 7
down 6
forward 6
forward 7
forward 1
forward 4
forward 2
forward 3
up 8
down 3
down 7
forward 6
forward 4
up 7
forward 6
forward 6
down 7
up 8
down 5
forward 6
forward 8
down 3
up 2
down 5
forward 2
forward 5
up 8
forward 1
down 3
forward 3
forward 2
down 3
down 8
forward 3
forward 1
down 5
down 1
up 1
forward 9
down 7
up 2
forward 8
down 6
down 5
up 9
forward 2
forward 5
forward 8
up 2
up 5
forward 2
down 2
down 9
down 3
forward 7
up 5
forward 7
down 6
forward 2
forward 7
forward 8
forward 8
down 7
forward 3
forward 6
down 5
forward 8
forward 6
up 2
forward 1
up 9
forward 1
up 3
forward 6
down 4
down 5
down 8
up 6
forward 1
down 8
forward 3
forward 2
forward 9
down 5
down 9
forward 5
down 7
up 9
forward 5
forward 7
forward 6
forward 5
down 3
forward 6
down 9
up 8
forward 4
forward 7
forward 3
down 7
forward 8
down 5
forward 3
up 6
up 5
forward 9
up 4
up 9
forward 9
forward 3
down 8
forward 8
down 3
forward 2
down 4
down 1
forward 2
up 9
down 7
forward 4
up 3
down 9
down 6
forward 2
forward 5
down 7
down 2
forward 8
down 5
forward 8
down 8
down 4
down 1
down 2
forward 5
down 8
down 1
down 2
forward 8
forward 3
down 8
up 8
up 8
down 3
forward 3
forward 6
down 9
up 1
forward 6
up 1
down 1
down 9
forward 3
up 1
forward 7
forward 6
forward 1
up 3
down 8
forward 7
down 3
down 5
down 7
forward 6
down 9
forward 9
forward 8
down 9
forward 1
down 2
up 7
down 3
down 1
forward 8
forward 4
forward 9
up 9
down 4
forward 1
down 1
up 1
up 1
up 6
down 7
down 5
forward 1
forward 7
up 3
down 7
up 3
down 4
up 9
up 9
forward 1
down 4
down 6
forward 2
forward 6
up 1
forward 1
down 8
forward 7
up 6
forward 6
forward 3
up 1
up 6
forward 1
down 2
forward 8
forward 4
forward 2
down 3
forward 2
forward 3
forward 1
down 6
forward 7
forward 7
down 4
forward 6
up 3
up 4
up 6
down 7
down 8
forward 3
down 2
forward 5
down 4
forward 6
forward 7
forward 8
forward 9
forward 3
down 1
forward 8
forward 1
down 8
up 1
down 3
down 6
down 1
up 1
forward 1
down 6
down 5
forward 6
down 1
down 5
forward 7
up 3
forward 4
forward 4
forward 1
up 6
up 2
up 4
down 4
up 4
forward 8
up 8
forward 1
down 5
forward 5
down 7
up 5
up 7
up 5
forward 9
down 1
down 1
forward 4
down 2
down 2
down 3
down 1
forward 1
up 7
forward 6
forward 9
up 5
forward 1
forward 9
up 2
forward 5
down 4
forward 6
down 9
down 3
forward 1
down 2
down 3
down 1
down 3
forward 8
up 6
forward 2
down 5
down 9
down 4
up 2
up 9
forward 2
down 7
forward 9
down 5
down 5
up 6
forward 1
forward 5
forward 9
down 4
forward 2
forward 7
down 2
forward 4
down 2
forward 3
down 3
down 2
up 5
forward 8
up 8
down 9
forward 9
down 9
down 4
down 1
forward 4
forward 9
down 5
down 9
down 4
down 5
forward 1
down 3
down 3
down 4
forward 6
forward 5
down 3
up 4
forward 9
forward 5
forward 3
forward 6
down 8
up 9
forward 2
up 6
forward 2
down 9
up 9
down 4
forward 1
forward 9
down 5
forward 9
forward 4
down 6
forward 7
forward 4
down 7
down 1
forward 9
down 6
down 5
forward 5
down 5
down 1
forward 3
down 7
down 5
down 9
down 5
up 6
up 5
down 5
up 1
down 9
forward 5
forward 9
forward 3
forward 4
down 7
forward 3
forward 3
down 5
forward 7
down 9
forward 8
forward 4
forward 8
forward 9
forward 1
forward 6
up 9
down 3
forward 1
forward 4
down 2
down 8
up 4
down 4
forward 1
down 5
down 3
down 9
up 1
forward 8
down 6
down 4
forward 3
down 8
down 2
up 6
down 5
forward 8
down 4
up 1
forward 5
down 1
down 9
down 1
down 9
down 3
down 3
forward 2
forward 6
down 8
forward 1
up 4
down 3
forward 9
up 2
down 4
forward 9
down 3
down 1
down 3
down 4
up 6
down 2
forward 3
forward 9
forward 7
down 2
down 5
forward 4
forward 5
down 9
up 3
forward 5
forward 9
up 2
forward 3
down 4
forward 2
down 5
down 8
down 1
forward 4
up 4
forward 7
down 9
forward 8
down 8
forward 3
down 6
up 9
up 6
down 2
forward 6
up 1
down 5
down 5
down 9
up 2
down 2
forward 1
forward 8
down 2
up 8
down 3
forward 2
down 1
down 5
down 5
up 4
forward 5))

(defvar day2-test '(
forward 5
down 5
forward 8
up 3
down 8
forward 2))

(defun day2-travel (instruct)
  (let ((distance 0)
        (depth 0))
    (maplist
     (lambda (i)
       (when (symbolp (car i))
         (let ((action (car i))
               (n (cadr i)))
           (cond
            ((eq action 'forward) (cl-incf distance n))
            ((eq action 'up)      (cl-decf depth n))
            ((eq action 'down)    (cl-incf depth n))
            (t (error "%s wtf?" action))))))
     instruct)
    (list distance depth (* distance depth))))

(day2-travel day2-test)
;; => (15 10 150)
(day2-travel day2-instruct)
;; => (2003 980 1962940)

(defun day2-travel-with-aim (instruct)
  "down X increases your aim by X units.
up X decreases your aim by X units.
forward X does two things:
  It increases your horizontal position by X units.
  It increases your depth by your aim multiplied by X."
  (let ((distance 0)
        (depth 0)
        (aim 0))
    (maplist
     (lambda (i)
       (when (symbolp (car i))
         (let ((action (car i))
               (n (cadr i)))
           (cond
            ((eq action 'forward)
             (cl-incf distance n)
             (cl-incf depth (* n aim)))
            ((eq action 'up)      (cl-decf aim n))
            ((eq action 'down)    (cl-incf aim n))
            (t (error "%s wtf?" action))))))
     instruct)
    (list distance depth aim (* distance depth))))

(day2-travel-with-aim day2-test)
;; => (15 60 10 900)
(day2-travel-with-aim day2-instruct)
;; => (2003 905474 980 1813664422)

(provide 'day2)
;;; day2.el ends here
