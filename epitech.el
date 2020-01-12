;;; epitech.el --- Epitech configuration for Emacs

;;; Author: Julien Philippon <julien.philippon@epitech.eu>
;;; Version: 0.1
;;; Keywords: epitech
;;; URL: https://github.com/Ersikan/epitech-emacs-package

;; Copyright (C) 2020 Julien Philippon, Jordan Demaison

;; MIT License

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package implements the standard Emacs configuration for Epitech students
;; Notably it provides a function (epitech-file-header) bound to C-c C-h to
;; insert a standard Epitech header

;;; Code:

;;;; Epitech header

;;;###autoload
(global-set-key (kbd "C-c C-h") 'epitech-file-header)

(defconst header-epitech-start "EPITECH PROJECT, "
    "Start of a standard Epitech header.")
(defconst header-epitech-description "File description:"
    "Start of file description of a standard Epitech header.")

(defun insert-with-one-whitespace (s)
    "Insert a string S in the buffer preceded by a single whitespace."
    ;; If the string is not empty
    (if (not (string= "" s))
        (insert
            ;; If the char just before point is a whitespace, do not insert one
            (if (string= " " (string (preceding-char)))
                s
                (concat " " s)))))

;; Alias for old function name
;;;###autoload
(defalias 'std-file-header 'epitech-file-header)

;; Inserts a standard Epitech header at the beginning of the file
;;;###autoload
(defun epitech-file-header ()
  "Puts a standard header at the beginning of the file.\n(According to mode)."
  (interactive)
   (save-excursion
   (let ((projname "")(projdescription ""))
    ;; Ask for project name
    (setq projname (read-from-minibuffer
                    (format "Type project name (RETURN to confirm): ")))
    ;; Ask for project description
    (setq projdescription
          (read-from-minibuffer
             (format "Type short file description (RETURN to confirm): ")))
       
    ;; Go to the start of the buffer, and add a empty line to separate
    ;; the header from the rest of the file
    (goto-char (point-min))
    (newline)
    (goto-char (point-min))

    ;; Start the comment that contains the header
    (insert comment-start)
    (newline-and-indent)
    
    ;; Insert the first line of the header
    (insert-with-one-whitespace (concat header-epitech-start
                     (format-time-string "%Y")))
    (newline-and-indent)
       
    ;; Inserts the project name
    (insert-with-one-whitespace projname)
    (newline-and-indent)
       
    ;; Inserts the file description header line
    (insert-with-one-whitespace header-epitech-description)
    (newline-and-indent)
       
    ;; Inserts the file description
    (insert-with-one-whitespace projdescription)
    (newline)
    
    ;; End correctly the comment of the header
    (if (string= comment-end "")
        (insert comment-start)
        (insert comment-end))
    (newline))))

;;;; Generating local keymaps for exotics modes.

;;; In CPerl mode, C-c C-h is used to do some help.
;;; so it is C-c C-h h
;;; For working, it requires info pages about perl
(add-hook 'cperl-mode-hook
          '(lambda ()
             (define-key cperl-mode-map (kbd "C-c C-h h")
               'epitech-file-header)))

(provide 'epitech)
;;; epitech.el ends here

