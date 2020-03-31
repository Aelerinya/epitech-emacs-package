;;; epitech.el --- Epitech configuration for Emacs -*- lexical-binding: t; -*-

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

(defun get-repository-name ()
  "Return the name of the git repository the current buffer is in.

Returns nil if not in a git repository."
  (with-temp-buffer
    (if (eq 0 (call-process "git" nil (current-buffer) nil
                            "rev-parse" "--show-toplevel"))
        (file-name-nondirectory (buffer-string)))))

;; Alias for old function name
;;;###autoload
(defalias 'std-file-header 'epitech-file-header)

(defun special-header-c-mode (name description)
  "Insert a special header for `c-mode'.

This header is ugly but coding style compliant
NAME is the name of the project
DESCRIPTION is the description of the project"
  (insert "/*")
  (newline)
  (insert (concat "** " header-epitech-start
                  (format-time-string "%Y")))
  (newline)
  (insert (concat "** " name))
  (newline)
  (insert (concat "** " header-epitech-description))
  (newline)
  (insert (concat "** " description))
  (newline)
  (insert "*/")
  (newline)
  (newline))

(defun has-shebang ()
  "Return t if the buffer start by a shebang."
  (if (>= (buffer-size) 2)
      (string= (buffer-substring (point-min) (+ (point-min) 2)) "#!")
    nil))

;; Inserts a standard Epitech header at the beginning of the file
;;;###autoload
(defun epitech-file-header (arg)
  "Puts a standard header at the beginning of the file according to the mode.

If ARG is present, asks for project name and description.
If not, insert git repo name as project name, and file name as description"
  (interactive "P")
  (save-excursion
    (let (
          ;; Get the project name (basename of the repo's root directory)
          (projname
           (let ((repo (get-repository-name)))
             (if repo (replace-regexp-in-string "\r?\n$" "" repo))))
          ;; Get the description : name of the current file
          (projdescription
           (if buffer-file-name (file-name-nondirectory buffer-file-name)))
          (comment-style 'extra-line)
          (comment-empty-lines 'eol)
          (header-min (point-min)))

      ;; If the universal argument is provided, or the value cannot
      ;; automatically be found, asks for a project name and a description

      ;; Ask for project name
      (if (or arg (not projname))
          (setq projname (read-from-minibuffer
                          (format "Type project name (RETURN to confirm): "))))
      ;; Ask for project description
      (if (or arg (not projdescription))
          (setq projdescription
                (read-from-minibuffer
                 (format "Type short file description (RETURN to confirm): "))))

      ;; Go to the start of the buffer
      (goto-char header-min)

      ;; If the buffer starts by a shebang, move one line down
      (if (has-shebang)
          (progn
            ;; Move down one line
            (forward-line)
            ;; Insert one more newline if the shebang does not end with one
            (if (/= (line-beginning-position) (point))
                (newline))
            ;; Separate the shebang and the header by an empty line
            (newline)
            ;; Save the starting position of the header
            (setq header-min (point))))

      ;; If the user is in C mode, insert the special
      ;; Coding Style compliant header
      (if (eq major-mode 'c-mode) (special-header-c-mode projname projdescription)
        (progn
          ;; Insert the first line of the header
          (insert (concat header-epitech-start
                          (format-time-string "%Y")))
          (newline)

          ;; Inserts the project name
          (insert projname)
          (newline)

          ;; Inserts the file description header line
          (insert header-epitech-description)
          (newline)

          ;; Inserts the file description
          (insert projdescription)
          (newline)

          ;; Comment the block
          (comment-region header-min (point))

          ;; If the comment end is the empty string, the comments ends at newlines
          ;; Therefore no comment extension will be put by setting `comment-style'
          ;; to `extra-lines'.
          ;;
          ;; We therefore need to insert the comment start string at
          ;; the start and the end of the comment block to achieve the desired
          ;; Epitech header look
          ;;
          ;; As the rest of the comment was commented using `comment-region',
          ;; we need to insert `comment-start' more times if `comment-add' is
          ;; different from 0
          (if (string= comment-end "")
              (let ((comment-start-adjusted
                     (apply 'concat
                            (make-list (+ 1 comment-add) comment-start))))
                (insert comment-start-adjusted)
                (newline)
                (save-excursion
                  (goto-char header-min)
                  (insert comment-start-adjusted)
                  (newline))))

          ;; Separate the header from the rest of the file
          (newline))))))
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

