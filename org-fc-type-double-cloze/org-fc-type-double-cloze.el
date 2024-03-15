;;; org-fc-type-double-cloze.el --- Cloze holes for double cards -*- lexical-binding: t; -*-

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
;; Cloze-holes on the back-side of double cards.
;;
;;; Code:

(require 'org-fc-core)
(require 'org-fc-review)
(require 'org-fc-type-cloze)

(defvar org-fc-type-double-cloze--hole-overlays '())
(defvar org-fc-type-double-cloze--text-overlays '())

(defun org-fc-type-double-cloze-setup ()
  (org-fc-review-with-current-item item
    (when
        (and
         (string= (oref item name) "back")
         (eq (oref (oref item card) type) 'double))
      (let ((end (org-fc-type-cloze--end))
            holes)
        (save-excursion
          (while (re-search-forward org-fc-type-cloze-hole-re end t)
            (push (match-data) holes)))
        (cl-loop
         for i below (length holes)
         for (hole-beg hole-end text-beg text-end hint-beg hint-end) in holes
         do
         (progn
           ;; Fake position if there is no hint
           (unless hint-beg (setq hint-beg text-end))
           (unless hint-end (setq hint-end text-end))
           (org-fc-hide-region hole-beg text-beg "")
           (remove-overlays text-beg text-end 'category 'org-fc)
           (push (org-fc-make-overlay text-beg text-end 'invisible t)
                 org-fc-type-double-cloze--text-overlays)
           (org-fc-hide-region text-end hint-beg "")
           (push (org-fc-overlay-surround
                  (org-fc-make-overlay hint-beg hint-end)
                  (concat "[" org-fc-type-cloze-hint-prefix)
                  "]"
                  'org-fc-type-cloze-hole-face)
                 org-fc-type-double-cloze--hole-overlays)
           (org-fc-hide-region hint-end hole-end "")
           (org-fc-make-overlay
            hole-beg hole-end
            'face 'org-fc-type-cloze-hole-face)))))))

(defun org-fc-type-double-cloze-flip ()
  (org-fc-review-with-current-item item
    (when
        (and
         (string= (oref item name) "back")
         (eq (oref (oref item card) type) 'double))
      (dolist (ov org-fc-type-double-cloze--text-overlays)
        (when (overlay-buffer ov)
          (overlay-put ov 'invisible nil)))
      (setq org-fc-type-double-cloze--text-overlays nil)
      (org-fc-show-latex)
      ;; Remove all overlays in the region of the hint to get rid of
      ;; latex overlays in the hint, then hide the region again.
      (dolist (ov org-fc-type-double-cloze--hole-overlays)
        (when (overlay-buffer ov)
          (let* ((hint-start (overlay-start ov))
                 (hint-end (overlay-end ov)))
            (remove-overlays hint-start hint-end)
            (org-fc-hide-region hint-start hint-end))))
      (setq org-fc-type-double-cloze--hole-overlays nil))))

(add-hook 'org-fc-after-setup-hook #'org-fc-type-double-cloze-setup)

(add-hook 'org-fc-after-flip-hook #'org-fc-type-double-cloze-flip)

(provide 'org-fc-type-double-cloze)
