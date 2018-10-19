;;; ry-org-scrum.el --- a simple org scrum package   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ryan Himmelwright

;; Author: Ryan Himmelwright <ryan.himmelwright@gmail.com>
;; Keywords: lisp org scrum
;; URL: https://github.com/himmAllRight/ry-org-scrum
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Just a simple package that contains a few functions to help
;; organize scrum planning in org mode.

;;; Code:

(provide 'ry-org-scrum)

;; Writting some org functions to help me manage my stuff easier.
(defclass ry/task ()
  ((level    :initarg :level    :initform 0)
   (status   :initarg :status   :initform nil)
   (heading  :initarg :heading  :initform "")
   (tags     :initarg :tags     :initform nil)
   (time     :initarg :time     :initform 0))
  (:documentation "Class for objects to hold info about each task."))


;; An alist to manage the status names and their respective columns in
;; the scrum table.
(setf *columns-alist* '(("Queue"      "\n| %s | | | | |")
			("Working On" "\n| | %s | | | |")
			("On Hold"    "\n| | | %s | | |")
			("Finished"   "\n| | | | %s | |")
			("Removed"    "\n| | | | | %s |")))

(defun ry/create-scrum-board ()
  "Create a new scrum board, with BEGIN and END tags."
  (interactive)
  (insert (format "#+BEGIN: scrum-board\n"))
  (ry/generate-scrum-board)
  (insert (format "\n#+END:")))

(defun org-dblock-write:scrum-board (params)
  "Add scrum-board as an org-dblock-write"
  (ry/generate-scrum-board))

(defun ry/generate-scrum-board ()
  "Generates a scrum board from the current buffer."
  (interactive)
  (let ((task-list (ry/get-tasks-with-status)))
    ;; Create Header
    (insert
     (format "|--|\n|%s|\n|--|"
	     (mapconcat #'car *columns-alist* " | ")))

    ;; Insert rows for each task
    (mapcar #'ry/insert-scrum-board-task
	    task-list)

    ;; Align compelted table
    (org-table-align)))

(defun ry/insert-scrum-board-task (task)
  (let* ((status       (oref task :status))
	 (link-heading (format "[[%s]]" (oref task :heading)))
	 (str          (ry/scrum-row-format-string status)))
    ;; Only write task it matches a proper status
    (when str
      (insert (format str link-heading)))))

(defun ry/scrum-row-format-string (status)
  "Returns the format string based on status/column placement
using *coumns-alist*."
  (let ((var (assoc status *columns-alist* #'string-equal)))
    (when var
      (cadr var))))



(defun ry/get-tasks-with-status ()
  "Returns a list of task objects in the buffer that have a
status marked."
  (interactive)
  (let (output-items)
    (org-map-entries
     (lambda ()
       (let* ((heading-data (org-heading-components))w
	      (level        (elt heading-data 0))
	      (status       (elt heading-data 2))
	      (heading      (elt heading-data 4))
	      (tags         (elt heading-data 5))
	      (time (org-element-property :TIME (org-element-at-point))))
	 (when status
	   (push
	    (make-instance 'ry/task
			   :level    level
			   :status   status
			   :heading  heading
			   :tags     tags
			   :time     (when time (string-to-number time)))
	    output-items))))
     nil
     nil)
    (reverse output-items)))

;;; ry-org-scrum.el ends here
