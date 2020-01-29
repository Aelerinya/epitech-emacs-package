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

;; Alias for old function name
;;;###autoload
(defalias 'std-file-header 'epitech-file-header)

;; Inserts a standard Epitech header at the beginning of the file
;;;###autoload
(defun epitech-file-header (arg)
  "Puts a standard header at the beginning of the file according to the mode.\n
If ARG is present, asks for project name and description.\n
If not, insert git repo name as project name, and file name as description"
  (interactive "P")
  (save-excursion
    (let (
          ;; Get the project name (basename of the repo's root directory)
          (projname
           (replace-regexp-in-string
            "\r?\n$" ""
            (shell-command-to-string "basename `git rev-parse --show-toplevel`")))
          ;; Get the description : name of the current file
          (projdescription (file-name-nondirectory buffer-file-name))
          (comment-style 'extra-line))

      ;; If the universal argument is provided, asks for a project name
      ;; and a description
      (if arg (progn
                ;; Ask for project name
                (setq projname (read-from-minibuffer
                                (format "Type project name (RETURN to confirm): ")))
                ;; Ask for project description
                (setq projdescription
                      (read-from-minibuffer
                       (format "Type short file description (RETURN to confirm): ")))))

      ;; Go to the start of the buffer
      (goto-char (point-min))

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
      (comment-region (point-min) (point))

      ;; Separate the header from the rest of the file
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

