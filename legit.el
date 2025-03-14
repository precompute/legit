;;; legit.el --- Navigate quickly across Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/legit
;; Created: March 12, 2025
;; Modified: March 12, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))

;; This program is free software: you can redistribute it and/or modify
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

;; Leg it across Emacs!  Frame, Window, Line, Word - traverse the
;; hierarchy and reach where you were supposed to really be!

;;; Code:
;;;; Faces
(defface legit-line-face
  '((t :inherit 'region
       :weight 'bold
       :box t))
  "Legit.el Face for lines.")

;;;; Variables
(defcustom legit-layout "asdfghjklqwertyuiopzxcvbnm"
  "String with layout to use for legit.el’s shortcuts.
For optimal ergonomics, define with home row keys at the beginning."
  :type 'string
  :group 'legit)

;;;; Functions
;;;;; Decimal To Base
(defun legit--decimal-to-base (num base)
  "Convert a NUM in decimal to a certain BASE and return a list."
  (let ((q (/ num base))
        (r (mod num base)))
    (cond ((>= q base) (list (legit--decimal-to-base q base) r))
          ((= q 0) (list r))
          (t (list q r)))))

;;;;; Clear Overlays
(defun legit--clear-overlays ()
  "Clear all overlays that contain `legit-overlay’."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'legit-overlay)
      (delete-overlay ov))))

;;;;; Make string for object at INDEX
(defun legit--get-obj-str (index padding letters)
  "Get a string using LETTERS for the current object at INDEX with PADDING.
Returns a string."
  (let ((s (thread-last (legit--decimal-to-base index (length letters))
                        flatten-tree
                        (mapcar #'(lambda (x) (aref letters x)))
                        (apply #'string))))
    (if (> padding (length s))
        (concat (make-string (- padding (length s)) (aref letters 0)) s)
      s)))

;;;;; Jump to a line
;;;;;; Get user input
(defun legit--input-n-chars (n)
  "Read exactly N chars from the user."
  (let (input)
    (dotimes (i n)
      (thread-last (read-char)
                   char-to-string
                   (concat input)
                   (setq input)))
    input))

;;;;;; Add overlays for legit-line
(defun legit--add-buffer-line-overlay ()
  "Makes overlays before a line in the current buffer.
Returns an alist of (shortcut . line number)."
  (save-excursion
    (goto-char (window-start))
    (let* ((lines-in-window (count-lines (window-start) (window-end)))
           (first-line-number (line-number-at-pos (window-start)))
           (line-index 0)
           (line-db (list))
           (pad (thread-last legit-layout
                             length
                             (log lines-in-window)
                             floor))
           (pad (if (= lines-in-window (length legit-layout))
                    pad
                  (1+ pad))))
      (while (< (point) (window-end))
        (let ((overlay (make-overlay (line-beginning-position) (line-beginning-position)))
              (line-str (legit--get-obj-str line-index pad legit-layout)))
          (overlay-put overlay 'legit-overlay t)
          (overlay-put overlay 'before-string (propertize line-str 'face 'legit-line-face))
          (setq line-db (cons (cons line-str (+ first-line-number line-index)) line-db)))
        (forward-line 1)
        (setq line-index (1+ line-index)))
      line-db)))

;;;;;; legit-jump
(defun legit-to-line ()
  "Create overlays in current buffer and jump to a line."
  (interactive)
  (unwind-protect
      (let* ((db (legit--add-buffer-line-overlay))
             (pad (length (caar db)))
             (target (legit--input-n-chars pad))
             (line (cdr (assoc target db))))
        (goto-char (point-min))
        (forward-line (1- line)))
    (legit--clear-overlays)))

;;; provide
(provide 'legit)

;;; legit.el ends here
