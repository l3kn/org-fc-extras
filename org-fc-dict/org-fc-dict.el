;;; org-fc-dict.el --- Dictionary interface for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2024  Leon Rische

;; Author: Leon Rische <emacs@leonrische.me>

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

;;; Dictionary Classes

(defclass org-fc-dict ()
  ((directory
    :initarg :directory
    :type string
    :documentation "Source directory.")
   (cache
    :initform nil
    :documentation "Cache of dictionary entries.")))

(defclass org-fc-dict-by-title (org-fc-dict) ())

(defclass org-fc-dict-by-key (org-fc-dict)
  ((property
    :initarg :property
    :type string
    :documentation "Property used for dictionary keys.")))

(cl-defmethod org-fc-dict-ensure-cache ((dict org-fc-dict))
  (when (null (oref dict cache))
    (org-fc-dict-build-cache dict)))

(cl-defmethod org-fc-dict-build-cache ((dict org-fc-dict-by-title))
  (let ((table (make-hash-table :test 'equal))
        (lines
         (split-string
          (shell-command-to-string
           (format
            "rg '^\\* (\\S( ?\\S)*)' -o -r '$1' %s"
            (oref dict directory)))
          "\n")))
    (dolist (line lines)
      (let ((parts (split-string line ":")))
        (puthash (cadr parts) (car parts) table)))
    (oset dict cache table)))

(cl-defmethod org-fc-dict-build-cache ((dict org-fc-dict-by-key))
  (let ((table (make-hash-table :test 'equal))
        (lines
         (split-string
          (shell-command-to-string
           (format
            "rg '^:%s: (.*)' -o -r '$1' %s"
            (oref dict property)
            (oref dict directory)))
          "\n")))
    (dolist (line lines)
      (let ((parts (split-string line ":")))
        (puthash (cadr parts) (car parts) table)))
    (oset dict cache table)))

(cl-defmethod org-fc-dict-lookup ((dict org-fc-dict-by-title))
  (interactive)
  (org-fc-dict-ensure-cache dict)
  (let* ((table (oref dict cache))
         (word
          (completing-read
           "Word: "
           (hash-table-keys table))))
    (when word
      (with-current-buffer (find-file (gethash word table))
        (goto-char (point-min))
        (search-forward (format "* %s" word))
        (switch-to-buffer (current-buffer))))))

(defvar org-fc-dict-track-lookup-count t)
(defvar org-fc-dict-lookup-count-property "FC_LOOKUP_COUNT")
(defvar org-fc-dict-lookup-count-threshold 3)
(defvar org-fc-dict-lookup-count-tag "freq")

(defun org-fc-dict--increment-lookup-count ()
  (when org-fc-dict-lookup-count
    (let ((lookup-count
           (string-to-number
            (or (org-entry-get nil org-fc-dict-lookup-count-property) "0"))))
      (when (and (= (1+ lookup-count)
                    org-fc-dict-lookup-count-threshold)
                 (not (org-fc-entry-p)))
        (org-fc--add-tag org-fc-dict-lookup-count-tag))
      (org-set-property
       org-fc-dict-lookup-count-property
       (number-to-string (1+ lookup-count))))))

(cl-defmethod org-fc-dict-lookup ((dict org-fc-dict-by-title))
  (interactive)
  (org-fc-dict-ensure-cache dict)
  (let* ((table (oref dict cache))
         (word
          (completing-read
           "Word: "
           (hash-table-keys table))))
    (when word
      (with-current-buffer (find-file (gethash word table))
        (goto-char (point-min))
        (search-forward (format "* %s" word))
        (org-fc-dict--increment-lookup-count)
        (switch-to-buffer (current-buffer))))))

(cl-defmethod org-fc-dict-lookup ((dict org-fc-dict-by-key))
  (interactive)
  (org-fc-dict-ensure-cache dict)
  (let* ((table (oref dict cache))
         (word
          (completing-read
           "Word: "
           (hash-table-keys table))))
    (when word
      (with-current-buffer (find-file (gethash word table))
        (goto-char (point-min))
        (search-forward (format ":%s: %s" (oref dict property) word))
        (org-back-to-heading)
        (org-fc-dict--increment-lookup-count)
        (switch-to-buffer (current-buffer))))))

(let ((my-dict (org-fc-dict-by-key
                :directory "/home/leon/org/flashcards/chinese/cedict/"
                :property "CE_KEY"
                )))
  (org-fc-dict-ensure-cache my-dict)
  (org-fc-dict-lookup my-dict))

;;; Macros

(defmacro org-fc-dict-define (name dict)
  `(progn
     (setq ,name ,dict)
     (defun ,(intern (format "%s-lookup" name)) ()
       (interactive)
       (org-fc-dict-lookup ,name))))

(provide 'org-fc-dict)
