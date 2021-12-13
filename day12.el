;;; day12.el --- Advent of Code 2021 -- Day 12: Passage Pathing  -*- lexical-binding: t; -*-

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

;; Day 12: Passage Pathing

;;; Code:

(defvar day12-test1 '( ;; 10 paths
start-A
start-b
A-c
A-b
b-d
A-end
b-end
))

(defvar day12-test2 '( ;; 19 paths
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
))

(defvar day12-test3 '( ;; 226 paths
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
))

(defvar day12-data '(
re-js
qx-CG
start-js
start-bj
qx-ak
js-bj
ak-re
CG-ak
js-CG
bj-re
ak-lg
lg-CG
qx-re
WP-ak
WP-end
re-lg
end-ak
WP-re
bj-CG
qx-start
bj-WP
JG-lg
end-lg
lg-iw
))

(defun day12-load (cave)
  "Load \"xx-yy\" paths in CAVE to ((xx . yy) ...)."
  (mapcar
   (lambda (p)
     (let* ((ps (split-string (symbol-name p) "-"))
            (p1 (car ps))
            (p2 (cadr ps)))
       (cons p1 p2)))
   cave))

(defun day12-adjacent (cave node)
  "Return list of all adjecent node in CAVE to NODE."
  (mapcar
   #'cdr
   (apply
    #'append
    (mapcar
     (lambda (n)
       (let (out)
         (when (string-equal node (car n))
           (cl-pushnew n out :test #'equal))
         (when (string-equal node (cdr n))
           (cl-pushnew (cons (cdr n) (car n)) out :test #'equal))
         out))
     cave))))

(day12-adjacent (day12-load day12-test1) 'start)
;; => ("A" "b")

(defun day12a-allowed (path next)
  "Can NEXT be visited given our PATH?

Lowercase location names can only be visited once."
  (if (string-equal next (downcase next))
      (not (member next path))
    t))

(defun day12a-tree (cave path)
  "Return tree of CAVE from the first entry in PATH to \"end\"."
  (cons (car path)
        (let (paths)
          (mapc
           (lambda (n)
             (if (string-equal n "end")
                 (push (list n) paths)
               (if (day12a-allowed path n)
                   (push (day12a-tree cave (cons n path)) paths))))
           (day12-adjacent cave (car path)))
          paths)))

(day12-tree (day12-load day12-test1) '("start"))

(defun day12-walk-tree (tree)
  "List all the paths in TREE."
  (if (atom tree)
      (list tree)
    (if (cdr tree)
        (let (paths)
          (dolist (b (cdr tree))
            (dolist (p (day12-walk-tree b))
              (push (cons (car tree) p) paths)))
          paths)
      (list tree))))

(defun day12a-path (cave)
  "Return paths in CAVE."
  (cl-delete-if
   (lambda (p) (not (equal (last p) '("end"))))
   (day12-walk-tree (day12a-tree (day12-load cave) '("start")))))

(defun day12a-count-of-paths (cave)
  "Return the number of paths thru CAVE."
  (length (day12a-path cave)))

(day12a-count-of-paths day12-test1)
;; => 10
(day12a-count-of-paths day12-test2)
;; => 19
(day12a-count-of-paths day12-test3)
;; => 226
(day12a-count-of-paths day12-data)
;; => 3230

(defun day12b-small-room-visits (path)
  "Return the max count of visits to a small room on PATH."
  (let (rooms)
    (dolist (r path)
      (when (string-equal r (downcase r))
        (setf (alist-get r rooms nil nil #'string-equal) (1+ (alist-get r rooms 0 nil #'string-equal)))))
    (apply #'max (mapcar #'cdr rooms))))

(defun day12b-allowed (path next)
  "Can NEXT be visited given our PATH?

Lowercase location names can only be visited tw1ce if no other
has been visited twice."
  (cond
    ;; visit start once
   ((string-equal next "start") nil)
   ;; uppercase are large and can be revisited
   ((not (string-equal next (downcase next))) t)
   ;; must be small but if this is our first time
   ((not (member next path)) t)
   ;; we've been here but has any other small room been visited twice already
   ((= (day12b-small-room-visits path) 1) t)
    (t nil)))

(defun day12b-tree (cave path)
  "Return tree of CAVE from the first entry in PATH to \"end\"."
  (cons (car path)
        (let (paths)
          (mapc
           (lambda (n)
             (if (string-equal n "end")
                 (push (list n) paths)
               (if (day12b-allowed path n)
                   (push (day12b-tree cave (cons n path)) paths))))
           (day12-adjacent cave (car path)))
          paths)))


(defun day12b-path (cave)
  "Return paths in CAVE."
  (cl-delete-if
   (lambda (p) (not (equal (last p) '("end"))))
   (day12-walk-tree (day12b-tree (day12-load cave) '("start")))))

(defun day12b-count-of-paths (cave)
  "Return the number of paths thru CAVE."
  (length (day12b-path cave)))

(day12b-count-of-paths day12-test1)
;; => 36
(day12b-count-of-paths day12-test2)
;; => 103
(day12b-count-of-paths day12-test3)
;; => 3509
(day12b-count-of-paths day12-data)
;; => 83475

(provide 'day12)
;;; day12.el ends here
