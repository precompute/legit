;;; legit.el --- Navigate quickly across Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/legit
;; Created: March 12, 2025
;; Modified: January 19, 2026
;; Version: 0.0.12
;; Package-Requires: ((emacs "26.1"))

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
(require 'subr-x)
;;;; Faces
(defface legit-word-jump-face
  '((t :inherit 'region))
  "Legit.el Face for words.")

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
;;;;; Utility Functions
;;;;;; Decimal To Base
(defun legit--decimal-to-base (num base)
  "Convert a NUM in decimal to a certain BASE and return a nested list."
  (let ((q (/ num base))
        (r (mod num base)))
    (cond ((>= q base) (list (legit--decimal-to-base q base) r))
          ((= q 0) (list r))
          (t (list q r)))))

;;;;;; Clear Overlays
(defun legit--clear-overlays-in-buffer ()
  "Clear all overlays in the current buffer that contain `legit-overlay’."
  (cl-loop for ov being the overlays of (current-buffer)
           do (when (overlay-get ov 'legit-overlay)
                (delete-overlay ov))))

(defun legit--clear-overlays-in-frame ()
  "Clear all overlays in all windows of the selected frame that contain `legit-overlay’."
  (cl-loop for w being the windows of (selected-frame)
           do (with-selected-window w (legit--clear-overlays-in-buffer))))

;;;;;; Make string for object at INDEX
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

;;;;;; Get user input
(defun legit--input-n-chars (n letters)
  "Read N chars from the user.  Return nil if N is not in LETTERS."
  (cl-loop repeat n
           for c = (read-char)
           if (cl-position c letters)
           concat (char-to-string c) into result
           else return nil
           finally return result))

;;;;;; Padding
(defun legit--pad (l)
  "Determine padding for sequence of length L."
  ((lambda (x) (if (= l (length legit-layout)) x (1+ x)))
   (thread-last legit-layout length (log l) floor)))

;;;;; Jump to a word
;;;;;; Add overlays for legit-to-word
(defun legit--add-word-overlay ()
  "Makes overlays above words in the selected line.
Returns an alist of (shortcut . word number)."
  (save-excursion
    (let* ((line-data (let ((sentence (list))
                            (num 0)
                            pos
                            (pos-db (list)))
                        (save-excursion
                          (save-restriction
                            (narrow-to-region (line-beginning-position) (line-end-position))
                            (beginning-of-line)
                            (setq pos (point))
                            (while (forward-word-strictly 1)
                              (setq sentence (nconc sentence (list (buffer-substring-no-properties pos (point)))))
                              (setq pos-db (cons pos pos-db))
                              (setq pos (point))
                              (cl-incf num))))
                        (cons (append (list sentence) num) (reverse pos-db))))
           (target-line (caar line-data))
           (words-in-line (cdar line-data))
           (word-db (cdr line-data)))
      (when (> words-in-line 1)
        (let* ((str-db (list))
               (w (selected-window))
               (pad (legit--pad words-in-line))
               (overlay-str (concat
                             (cl-loop for word-index from 0 to (1- words-in-line)
                                      concat ((lambda (a b)
                                                ((lambda (s)
                                                   (setq str-db (cons b str-db))
                                                   (concat s (make-string (- (length a) (length s)) 32)))
                                                 (replace-regexp-in-string "^\\([[:space:]]*\\).*"
                                                                           (concat "\\1" b) a)))
                                              (pop target-line)
                                              (legit--get-obj-str word-index pad legit-layout)))
                             "\n")))
          (let ((overlay (make-overlay (line-beginning-position) (line-beginning-position))))
            (overlay-put overlay 'legit-overlay t)
            (overlay-put overlay 'before-string (propertize overlay-str 'face 'legit-word-jump-face))
            (overlay-put overlay 'window w))
          (cl-loop for x in (reverse str-db) for y in word-db collect (cons x y)))))))

;;;;;; legit-to-word
(defun legit-to-word ()
  "Create overlays in current line and jump to a word."
  (interactive)
  (unwind-protect
      (when-let* ((db (legit--add-word-overlay))
                  (pad (length (caar db)))
                  (target (legit--input-n-chars pad legit-layout))
                  (word (cdr (assoc target db))))
        (goto-char word)
        (when (= (point) (line-beginning-position)) (beginning-of-line-text)))
    (legit--clear-overlays-in-buffer)))

;;;;; Jump to a line
;;;;;; Add overlays for legit-to-line
(defun legit--add-before-line-overlay ()
  "Makes overlays before a line in the current buffer.
Returns an alist of (shortcut . line number)."
  (save-excursion
    (goto-char (window-start))
    (let* ((lines-in-window (count-lines (window-start) (window-end)))
           (first-line-number (line-number-at-pos (window-start)))
           (line-index 0)
           (line-db (list))
           (w (selected-window))
           (pad (legit--pad lines-in-window)))
      (when (> lines-in-window 1)
        (while (< (point) (window-end))
          (let ((overlay (make-overlay (line-beginning-position) (line-beginning-position)))
                (line-str (legit--get-obj-str line-index pad legit-layout)))
            (overlay-put overlay 'legit-overlay t)
            (overlay-put overlay 'before-string (propertize (concat line-str " ")
                                                            'face 'legit-line-jump-face))
            (overlay-put overlay 'window w)
            (setq line-db (cons (cons line-str (+ first-line-number line-index)) line-db)))
          (forward-line 1)
          (cl-incf line-index)))
      line-db)))

;;;;;; legit-to-line
(defun legit-to-line ()
  "Create overlays in current buffer and jump to a line."
  (interactive)
  (unwind-protect
      (when-let* ((db (legit--add-before-line-overlay))
                  (pad (length (caar db)))
                  (target (legit--input-n-chars pad legit-layout))
                  (line (cdr (assoc target db))))
        (goto-line line)
        (when legit-jump-skip-leading-spaces
          (beginning-of-line-text)))
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
           (pad (legit--pad windows-in-frame)))
      (when (> windows-in-frame 1)
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
        window-db))))

;;;;;; legit-to-window
(defun legit-to-window ()
  "Create overlays in visible windows on current frame and jump to one."
  (interactive)
  (unwind-protect
      (when-let* ((db (legit--add-window-overlay))
                  (pad (length (caar db)))
                  (target (legit--input-n-chars pad legit-layout))
                  (window (cdr (assoc target db))))
        (select-window window))
    (legit--clear-overlays-in-frame)))

;;;;; Jump to a frame
;;;;;; Make collection for completing-read
(defun legit--frame-collection ()
  "Return a collection that comprises of various properties of all emacs
frames and a composed string of the same."
  (let* ((frame-data (cl-loop for f being the frames collect f)) ;; use `frames-on-display-list’ instead?
         (number-of-frames (length frame-data)))
    (when (> number-of-frames 1)
      (let* ((pad (legit--pad number-of-frames))
             (frame-strs (mapcar (lambda (x) (legit--get-obj-str x pad legit-layout))
                                 (number-sequence 0 (1- number-of-frames))))
             (frame-wspc-nums (if (and (eq system-type 'gnu/linux)
                                       (equal (window-system) 'x))
                                  (mapcar (lambda (x)
                                            (aref (x-window-property "_NET_WM_DESKTOP" x "CARDINAL") 0))
                                          frame-data)
                                (make-list number-of-frames "")))
             (frame-windows (mapcar (lambda (x)
                                      (let* ((w (cl-loop for w being the windows of x
                                                         collect (with-selected-window w major-mode)))
                                             (u-w (seq-uniq w)))
                                        (cl-loop for u in u-w
                                                 concat (concat (propertize (format "%sx" (cl-count u w)) 'face 'success)
                                                                (propertize ((lambda (z)
                                                                               (if (string-suffix-p "-mode" z)
                                                                                   (substring z 0 -5)
                                                                                 z))
                                                                             (format "%s" u))
                                                                            'face 'font-lock-builtin-face)))))
                                    frame-data))
             (frame-names (mapcar (lambda (x) (frame-parameter x 'name)) frame-data))
             (frame-db (cl-loop for a in frame-strs
                                for b in frame-wspc-nums
                                for c in frame-windows
                                for d in frame-names
                                for e in frame-data
                                collect (list a b c d e)))
             (frame-db-strs (cl-loop for a in frame-db
                                     collect (concat
                                              (propertize (car a) 'face 'region) " "
                                              (propertize (if (and (stringp (nth 1 a))
                                                                   (string-empty-p (nth 1 a)))
                                                              ""
                                                            (format "Wspc%s " (nth 1 a)))
                                                          'face 'font-lock-constant-face)
                                              (nth 2 a) " "
                                              (propertize (nth 3 a) 'face 'font-lock-doc-face)))))
        (cons frame-db frame-db-strs)))))

;;;;;; legit-to-frame
(defun legit-to-frame ()
  "Collect information about available frames and jump to one with `completing-read’.
If you’re running X on Gnu/Linux AND if the wmctrl program is accessible,jump
to workspace and then window.  Else bring window to current workspace."
  (interactive)
  (when-let* ((rcons (legit--frame-collection))
              (db (car rcons))
              (coll (cdr rcons))
              (result (completing-read "Leg It to Frame: " coll nil t))
              (frame-data (nth 4 (cl-find-if (lambda (x)
                                               (string-equal (car (split-string result)) (car x)))
                                             db))))
    (if (and (eq system-type 'gnu/linux) (equal (window-system) 'x) (executable-find "wmctrl"))
        (call-process "wmctrl" nil nil nil "-i" "-a" (frame-parameter frame-data 'outer-window-id))
      (select-frame-set-input-focus frame-data))))

;;;;; Jump Functions
(defun legit-from-frame ()
  "Leg It to a frame, then to a window, then to a line, and then to a word."
  (interactive)
  (legit-to-frame)
  (legit-to-window)
  (legit-to-line)
  (legit-to-word))

(defun legit-from-window ()
  "Leg It to a window, then to a line, and then to a word."
  (interactive)
  (legit-to-window)
  (legit-to-line)
  (legit-to-word))

(defun legit-from-line ()
  "Leg It to a line, and then to a word."
  (interactive)
  (legit-to-line)
  (legit-to-word))

;;; provide
(provide 'legit)

;;; legit.el ends here
