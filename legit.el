;;; legit.el --- Navigate quickly across Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/legit
;; Created: March 12, 2025
;; Modified: March 16, 2025
;; Version: 0.0.3
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
(defface legit-line-jump-face
  '((t :inherit 'region))
  "Legit.el Face for lines.")

(defface legit-window-jump-face
  '((t :inherit 'region
       :height 2.0))
  "Legit.el Face for windows.")

;;;; Variables
(defcustom legit-layout "asdfghjklqwertyuiopzxcvbnm"
  "String with layout to use for legit.el’s shortcuts.
For optimal ergonomics, define with home row keys at the beginning."
  :type 'string
  :group 'legit)

(defcustom legit-jump-skip-leading-spaces t
  "When jumping to a line, skip leading spaces and set point to the
beginning of the first word."
  :type 'boolean
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
(defun legit--clear-overlays-in-buffer ()
  "Clear all overlays in the current buffer that contain `legit-overlay’."
  (cl-loop for ov being the overlays of (current-buffer)
           do (when (overlay-get ov 'legit-overlay)
                (delete-overlay ov))))

(defun legit--clear-overlays-in-frame ()
  "Clear all overlays in all windows of the selected frame that contain `legit-overlay’."
  (cl-loop for w being the windows of (selected-frame)
           do (with-selected-window w (legit--clear-overlays-in-buffer))))

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

;;;;; Get user input
(defun legit--input-n-chars (n letters)
  "Read N chars from the user.  Return nil if N is not in LETTERS."
  (cl-loop repeat n
           for c = (read-char)
           if (cl-position c letters)
           concat (char-to-string c) into result
           else return nil
           finally return result))

;;;;; Jump to a line
;;;;;; Add overlays for legit-to-line
(defun legit--add-buffer-line-overlay ()
  "Makes overlays before a line in the current buffer.
Returns an alist of (shortcut . line number)."
  (save-excursion
    (goto-char (window-start))
    (let* ((lines-in-window (count-lines (window-start) (window-end)))
           (first-line-number (line-number-at-pos (window-start)))
           (line-index 0)
           (line-db (list))
           (w (selected-window))
           (pad (thread-last legit-layout
                             length
                             (log lines-in-window)
                             floor))
           (pad (if (= lines-in-window (length legit-layout)) pad (1+ pad))))
      (while (< (point) (window-end))
        (let ((overlay (make-overlay (line-beginning-position) (line-beginning-position)))
              (line-str (legit--get-obj-str line-index pad legit-layout)))
          (overlay-put overlay 'legit-overlay t)
          (overlay-put overlay 'before-string (propertize (concat line-str " ")
                                                          'face 'legit-line-jump-face))
          (overlay-put overlay 'window w)
          ;; (overlay-put overlay 'face 'legit-line-jump-face) ;; Does not work.
          (setq line-db (cons (cons line-str (+ first-line-number line-index)) line-db)))
        (forward-line 1)
        (cl-incf line-index))
      line-db)))

;;;;;; legit-to-line
(defun legit-to-line ()
  "Create overlays in current buffer and jump to a line."
  (interactive)
  (unwind-protect
      (let* ((db (legit--add-buffer-line-overlay))
             (pad (length (caar db)))
             (target (legit--input-n-chars pad legit-layout))
             (line (if target (cdr (assoc target db)) nil)))
        (if (not line)
            (message "Sequence not found.")
          (goto-line line)
          (when legit-jump-skip-leading-spaces
            (beginning-of-line-text))))
    (legit--clear-overlays-in-buffer)))

;;;;; Jump to a window
;;;;;; Add overlays for legit-to-window
(defun legit--add-window-overlay ()
  "Makes overlays before the first line in all visible windows in the current frame.
Returns an alist of (shortcut . window)."
  (save-excursion
    (let* ((window-stats (cl-loop for x being the windows of (selected-frame) collect x))
           ;; cl-loop does not return minibuffer windows, similar to (walk-windows FUN nil).
           (windows-in-frame (length window-stats))
           (window-index 0)
           (window-db (list))
           (pad (thread-last legit-layout
                             length
                             (log windows-in-frame)
                             floor))
           (pad (if (= windows-in-frame (length legit-layout)) pad (1+ pad))))
      (walk-windows
       (lambda (w)
         (with-selected-window w
           (let ((overlay (make-overlay (window-start) (window-start)))
                 (window-str (legit--get-obj-str window-index pad legit-layout)))
             (overlay-put overlay 'legit-overlay t)
             (overlay-put overlay 'before-string (propertize window-str
                                                             'face 'legit-window-jump-face))
             (overlay-put overlay 'window w)
             (setq window-db (cons (cons window-str w) window-db)))
           (cl-incf window-index))))
      window-db)))

;;;;;; legit-to-window
(defun legit-to-window ()
  "Create overlays in visible windows on current frame and jump to one."
  (interactive)
  (unwind-protect
      (let* ((db (legit--add-window-overlay))
             (pad (length (caar db)))
             (target (legit--input-n-chars pad legit-layout))
             (window (if target (cdr (assoc target db)) nil)))
        (if (not window)
            (message "Sequence not found.")
          (select-window window)))
    (legit--clear-overlays-in-frame)))

;;; provide
(provide 'legit)

;;; legit.el ends here
