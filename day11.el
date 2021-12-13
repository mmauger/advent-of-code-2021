;;; day11.el --- Advent of Code 2021 -- Day 11: Dumbo Octopus  -*- lexical-binding: t; -*-

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

;; Day 11: Dumbo Octopus

;;; Code:

(defvar day11-test '(
"5483143223"
"2745854711"
"5264556173"
"6141336146"
"6357385478"
"4167524645"
"2176841721"
"6882881134"
"4846848554"
"5283751526"
))

(defvar day11-data '(
"4658137637"
"3277874355"
"4525611183"
"3128125888"
"8734832838"
"4175463257"
"8321423552"
"4832145253"
"8286834851"
"4885323138"
))

(defun day11-cell (octo r c &optional dr dc)
  "Return value from R,C in OCTO."
  (let ((R (+ r (or dr 0)))
        (C (+ c (or dc 0))))
    (if (and (>= R 0) (< R (length octo))
             (>= C 0) (< C (length (car octo))))
        (aref (elt octo r) c)
      ?0)))

(defun day11-incr (octo r c &optional dr dc)
  "Incremement the cell at R,C in OCTO."
  (let ((R (+ r (or dr 0)))
        (C (+ c (or dc 0))))
    (if (and (>= R 0) (< R (length octo))
             (>= C 0) (< C (length (car octo))))
        (aset (elt octo r) c
              (1+ (aref (elt octo r) c)))
      ?0)))

(defun day11-reset (octo r c &optional dr dc)
  "Reset the cell at R,C in OCTO to 0."
  (let ((R (+ r (or dr 0)))
        (C (+ c (or dc 0))))
    (when (and (>= R 0) (< R (length octo))
               (>= C 0) (< C (length (car octo))))
      (aset (elt octo r) c ?0))))

(defun day11-age (octo)
  "Move OCTO thru one turn, return list of flashes."
  (let ((max-r (length octo))
        (max-c (length (car octo)))
        flash)
    (dotimes (r max-r)
      (dotimes (c max-c)
        (if (< (day11-cell octo r c) ?9)
            (day11-incr octo r c)
          (cl-pushnew (cons r c) flash :test #'equal)
          (day11-reset octo r c))))
    flash))

(defun day11-cycle (octo)
  "Move OCTO thru one time unit, and then handle flashes."
  (let ((flash (day11-age octo))
        (nflash 0))
    (while flash
      (let* ((f (pop flash))
             (r (car f))
             (c (cdr f)))
        (cl-incf nflash)
        (dolist (dr '(-1 0 1))
          (dolist (dc '(-1 0 1))
            (let* ((R (+ r (or dr 0)))
                   (C (+ c (or dc 0)))
                   (cell (day11-cell octo R C)))
              (if (= cell ?0)
                  nil ; don't pickup strength
                (if (< cell ?9)
                    (day11-incr octo R C)
                  (cl-pushnew (cons R C) flash :test #'equal)
                  (day11-reset octo R C))))))))
    nflash))

(defun day11a-100-cycles (octo &optional n)
  "Run thru 100 cycles with OCTO and return number of flashes."
  (let ((nflashes nil))
    (dotimes (i (or n 100))
      (push (day11-cycle octo) nflashes))
    (append (list (apply #'+ nflashes)) octo nflashes)))

;; try each cycle one at a time and compare to web page

(setq day11-test '("5483143223" "2745854711" "5264556173" "6141336146" "6357385478" "4167524645" "2176841721" "6882881134" "4846848554" "5283751526"))
(day11a-100-cycles day11-test 100)
;; => (1656 "0397666866" "0749766918" "0053976933" "0004297822" "0004229892" "0053222877" "0532222966" "9322228966" "7922286866" "6789998766" 13 ...)
;; 10 runs => (204 "0481112976" "0031112009" "0041112504" "0081111406" "0099111306" "0093511233" "0442361130" "5532252350" "0532250600" "0032240000" 29 39 24 7 1 8 16 45 35 0)
(day11a-100-cycles day11-data 100)
(1686 "9444570097" "4445700009" "4446000800" "4447007570" "4447008573" "4447332193" "4447611114" "5546511114" "1755711153" "1154571532" 16 ...)

(defun day11b-first-all-flash (octo)
  "Run thru 100 cycles with OCTO and return number of flashes."
  (let ((all-count (apply #'+ (mapcar #'length octo)))
        (d 1))
    (catch 'all-flash
      (while t
        (when (= (day11-cycle octo) all-count)
          (throw 'all-flash d))
        (cl-incf d)))))

(day11b-first-all-flash day11-test)
;; => 195
(day11b-first-all-flash day11-data)
;; => 360

(provide 'day11)
;;; day11.el ends here
