(require 'org)

(defvar scio-folder nil)
(defvar scio-student nil)
(defvar scio-comment nil)

(defun scio-set-folder ()
  (setq scio-folder (org-entry-get-with-inheritance "ARCHIVE")))

(defun scio-mark-student ()
  (let ((file (concat scio-folder "grades.csv"))
        (student-id (substring scio-student -9 -1)))
    (with-temp-buffer
      (insert-file-contents file)
      (search-forward student-id)
      (move-end-of-line 1)
      (search-backward ",")
      (kill-line)
      (insert ",Markerad")
      (write-file file))))

(defun scio-org-get-data-from-entry ()
  (setq scio-student (buffer-substring (+ (line-beginning-position) (org-current-level) 1)
                                       (line-end-position)))
  (org-show-subtree)
  (org-mark-subtree)
  (next-line)
  (kill-ring-save (point) (mark))
  (with-temp-buffer
    (yank)
    (mark-whole-buffer)
    (org-html-convert-region-to-html)
    (setq scio-comment (buffer-substring (point-min) (point-max))))
  (scio-set-folder))

(defun scio-copy-org-to-folder ()
  (interactive)
  (scio-org-get-data-from-entry)
  (let ((file (concat scio-folder scio-student "\\comments.txt")))
    (write-region scio-comment nil file)
    (org-forward-heading-same-level 1))
  (scio-mark-student))

(provide 'scio)
