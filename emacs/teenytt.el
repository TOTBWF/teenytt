;;; teenytt ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defgroup teenytt nil "teenytt" :prefix 'teenytt :group 'languages)

(defcustom teenytt-command "stack exec teenytt-exe --"
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

  (define-key teenytt-mode-map (kbd "C-c C-l") 'teenytt-load-buffer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.teenytt\\'" . teenytt-mode))

(provide 'teenytt)
;;; teenytt.el ends here
