(defun replace-placeholders (template-map template-string)
  "Replace placeholders in TEMPLATE-STRING with content from TEMPLATE-MAP."
  (let ((result template-string))
    (maphash
     (lambda (key value)
       (setq result (replace-regexp-in-string (format "{{\\s-*%s\\s-*}}" (regexp-quote key)) value result)))
     template-map)
    result))

(defun read-template-file (file-path)
  "Read the content of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun replace-placeholders-from-file (template-map file-path)
  "Replace placeholders in the content of FILE-PATH with content from TEMPLATE-MAP."
  (let ((template-string (read-template-file file-path)))
    (replace-placeholders template-map template-string)))

(defun build-org-content-map ()
  "Build a map from the structure of the current Org mode buffer."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let ((contents (make-hash-table :test 'equal)))
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (headline)
            (let* ((headline-title (car (org-element-property :title headline)))
                   (begin (org-element-property :contents-begin headline))
                   (end (org-element-property :contents-end headline))
                   (content (buffer-substring-no-properties begin end)))
              (puthash headline-title content contents))))
        contents)
    (error "This function can only be used in Org mode buffers.")))

(defun org-and-template-to-result ()
  "Fill a template with content from the current Org mode buffer and save the result."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((org-content-map (build-org-content-map))
             (template-file-path "template.tex"))
        (if (file-exists-p template-file-path)
            (let* ((base-name (file-name-sans-extension (file-name-nondirectory template-file-path)))
                   (output-file-path (format "%s/%s_result.%s"
                                             default-directory
                                             base-name
                                             (file-name-extension template-file-path))))
              (with-temp-buffer
                (insert (replace-placeholders-from-file org-content-map template-file-path))
                (write-file output-file-path)
                (message "Filled template saved to: %s" output-file-path)))
          (message "Template file does not exist.")))
    (error "This function can only be used in Org mode buffers.")))


