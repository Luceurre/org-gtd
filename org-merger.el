;;; org-merger.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pierre Glandon
;;
;; Author: Pierre Glandon <https://github.com/pglandon>
;; Maintainer: Pierre Glandon <pglandon78@gmail.com>
;; Created: February 27, 2021
;; Modified: February 27, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/pglandon/org-merger
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun get-headings-position (org-buffer level)
  "Return an a-list with key the name of the heading at LEVEL in the ORG-BUFFER and value a list of their point (position) in the file."
  (let ((level (or level 1))
        (result))
    (with-current-buffer org-buffer
      (org-map-entries
       (lambda () (let ((heading (org-element-property :title (org-element-at-point))))
                    (if (assoc heading result)
                        (push (point) (cdr (assoc heading result)))
                      (map-put result heading (list (point))))))
       (concat "+LEVEL=" (number-to-string level))))
    result))

(defun merge--headings (level)
  "Merge headings with the same name in current buffer at LEVEL.
Side effects include entries sorting by alphabetical order."
                                        ; We sort headings at cursor position.
  (ignore-errors (org-sort-entries t ?a))

                                        ; We merge subtrees if same headings.
  (let* ((level level) (headings (get-headings-position (current-buffer) level)))
    (dolist (heading headings)
      (let ((positions (sort (cdr heading) '<)))
        (when (> (length positions) 1)
          (let ((to-merge (cdr positions)))
            (dolist (to-merge-element to-merge)
              (goto-char to-merge-element)
              (kill-whole-line)))))))

                                        ; We do this again for each subtree.
  (org-map-entries
   (lambda () (progn
                (org-narrow-to-element)
                (merge--headings (+ 1 level))
                (widen)))
   (concat "+LEVEL=" (number-to-string level))))

(defun merge-headings (org-buffer)
  "Merge headings in ORG-BUFFER."
  (with-current-buffer org-buffer
    (goto-char (point-min))
    (merge--headings 1)))

(get-headings-position "test-file.org" 1)
(merge-headings "test-file.org")
(provide 'org-merger)
;;; org-merger.el ends here
