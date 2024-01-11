;;; gptel-project.el --- A description of the Gptel Project plugin

;; Author: isaacphi <your-email@example.com>
;; Version: 0.1.0
;; Keywords: convenience, ai
;; Package-Requires ((emacs "26.1"))
;; URL: http://github.com/isaacphi/gptel-project

;;; Commentary:

;; gptel-project is an Emacs extension that allows for collaboration
;; with Chat GPT inside Emacs. It leverages the gptel package to handle
;; communication with OpenAI's API.
                                        ;
;;; Code:
(require 'gptel)
(require 'projectile)

(defun gptel-project-setup-with-files ()
  (interactive)
  ;; Check if Projectile is loaded (featurep 'projectile)
  ;; and if the current buffer is in a Projectile project (projectile-project-p).
  (unless (and (featurep 'projectile) (projectile-project-p))
    (error "Projectile is not available, or you're not in a Projectile project"))

  (let* ((buffer-name (read-string "Enter buffer name: " "*GPTel Project*"))
         (buffer (get-buffer buffer-name)))
    ;; If buffer exists, pop to the bottom and end the function.
    (if buffer
        (progn
          (pop-to-buffer buffer)
          (goto-char (point-max))
          ;; TEMPORARY: always go to this buffer and clear it
          (erase-buffer))
      ;; (return))
      ;; Else, create the buffer.
      (setq gptel-buffer (get-buffer-create buffer-name))
      (with-current-buffer gptel-buffer
        (org-mode)
        (gptel-mode))))

  ;; If a new buffer is created, continue with setup.
  (when (buffer-live-p gptel-buffer)
    ;; Within the buffer, start inserting the content.
    (with-current-buffer gptel-buffer
      (insert "* Project Name:" (projectile-project-name) "\n")
      (insert "You will be provided with the full text of files in a software projet. You are to act as a helpful software engineer, providing concise answers about this project.\n")
      (insert "** Files:\n")
      (let ((project-files (projectile-project-files (projectile-project-root))))
        (dolist (file project-files)
          (let ((file-type (file-name-extension file)))
            (insert "*** " file "\n")
            (condition-case err
                ;; TODO: use list of files other than org-babel-tangle-lang-exts
                (if (member file-type (mapcar 'cdr org-babel-tangle-lang-exts))
                    (progn
                      (insert "#+BEGIN_SRC " (or (car (rassoc file-type org-babel-tangle-lang-exts)) "text") "\n")
                      (let ((file-contents (with-temp-buffer
                                (insert-file-contents (expand-file-name file (projectile-project-root)))
                                (buffer-string))))
                        (if (string= file-type "org")
                            (insert (replace-regexp-in-string "^\*" ",\*" file-contents))
                          (insert file-contents)))
                      ;; (insert (with-temp-buffer
                      ;;           (insert-file-contents (expand-file-name file (projectile-project-root)))
                      ;;           (buffer-string)))
                      (insert "#+END_SRC\n\n")))
              (error (insert "Error: Could not load the file contents. " (error-message-string err) "\n")))
            )))
      ;; After the file list, prepare the buffer for chat input from the user.
      (insert (gptel-prompt-prefix-string))
      ;; Ask the AI to summarize each file
      (insert "Following the same heading format as above, summarize each file. More Important files should have longer summaries but none should be more than a couple of paragraphs. Keep them as sort as possible. Also summarize the interface of each file at the bottom: what are the important functions and constants? ")
      (goto-char (point-max))) ;; Move the cursor to the end of the buffer.

    ;; Finally, display the newly populated buffer to the user.
    (pop-to-buffer gptel-buffer)))


(provide 'gptel-project)

;;; gptel-project.el ends here
