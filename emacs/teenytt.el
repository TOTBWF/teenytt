;;; teenytt.el --- Major mode for editing teenytt files -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Reed Mullanix

;; Author: Reed Mullanix <reedmullanix@gmail.com>
;; Version: 0.0.1
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or

;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing TeenyTT code.
;; This mainly provides some helpers for enabling some options,
;; and loading files.

;;; Code:

(require 'compile)

(defgroup teenytt nil "teenytt" :prefix 'teenytt :group 'languages)

(defcustom teenytt-command "cabal exec teenytt --"
  "The command to be run for teenytt."
  :group 'teenytt
  :type 'string
  :tag "Command for teenytt")

(defcustom teenytt-load-options nil
  "The options to provide to teenytt when loading a file."
  :group 'teenytt
  :type '(repeat string)
  :tag "Load options for teenytt")

(define-compilation-mode teenytt-compilation-mode "TeenyTT"
  "Cooltt specific `compilation-mode' derivative."
  ;; TODO: Error highlighting regexes
  )

(defun teenytt-toggle-debug ()
  "Toggle TeenyTT's debug mode."
  (interactive)
  (if (member "--debug" teenytt-load-options)
      (progn
	(setq teenytt-load-options (delete "--debug" teenytt-load-options))
	(message "TeenyTT Debug Mode Disabled."))
    (push "--debug" teenytt-load-options)
    (message "TeenyTT Debug Mode Enabled.")))


(defun teenytt-load-buffer ()
  "Load the current file into teenytt."
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (progn
        (when (buffer-modified-p) (save-buffer))
        (let* ((opts (mapconcat 'identity teenytt-load-options " "))
               (command (concat teenytt-command " load " filename " " opts)))
          (compilation-start command 'teenytt-compilation-mode nil t)))
    (error "Buffer has no file name!")))

(define-derived-mode teenytt-mode prog-mode "teenytt"
  "Major mode for TeenyTT."
  (set (make-local-variable 'comment-start) "-- ")

  (define-key teenytt-mode-map (kbd "C-c C-l") 'teenytt-load-buffer)
  (define-key teenytt-mode-map (kbd "C-c C-x C-d") 'teenytt-toggle-debug))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tt\\'" . teenytt-mode))

(provide 'teenytt)
;;; teenytt.el ends here
