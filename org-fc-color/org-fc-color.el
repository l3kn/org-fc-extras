(defun org-fc-color-remove-overlays ()
  "Remove all org-fc-color overlays in the current buffer."
  (interactive)
  (remove-overlays nil nil 'org-fc-color t))

(defun org-fc-color-blend (c1 c2 alpha)
  (cl-destructuring-bind (r g b)
      (cl-mapcar (lambda (v1 v2)
                   (+ (* v1 alpha) (* v2 (- 1 alpha))))
                 (color-name-to-rgb c1)
                 (color-name-to-rgb c2))
    (color-rgb-to-hex r g b 2)))

(defvar org-fc-color-colors nil)

(defun org-fc-color-choose (tags)
  (cl-loop for (tag-list . color) in org-fc-color-colors
           when (cl-subsetp tag-list tags :test #'string=)
           return color))

(defun org-fc-color-heading ()
  (interactive)
  (when-let ((color (org-fc-color-choose (org-get-tags-at))))
    (let* ((background (face-background 'default))
           (color-blend (org-fc-color-blend color background 0.3))
           (heading
            (save-excursion
              (org-back-to-heading)
              (cadr (org-element-at-point))))
           (begin (plist-get heading :begin))
           (end (plist-get heading :end))
           (ov (make-overlay begin end)))
      (overlay-put ov 'face `(:background ,color-blend :extend t))
      (overlay-put ov 'org-fc-color t))))

(defun org-fc-color-buffer ()
  (interactive)
  (org-fc-map-cards #'org-fc-color-heading))

(add-hook 'org-fc-before-setup-hook #'org-fc-color-heading)
(add-hook 'org-fc-after-rate-hook #'org-fc-color-remove-overlays)

(provide 'org-fc-color)
