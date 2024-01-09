;;; gptel-project.el --- A description of the Gptel Project plugin

;; Author: isaacphi <your-email@example.com>
;; Version: 0.1.0
;; Keywords: convenience, ai
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
          (return))
      ;; Else, create the buffer.
      (setq gptel-buffer (get-buffer-create buffer-name))
      (with-current-buffer gptel-buffer
        (org-mode)
        (gptel-mode))))

  ;; If a new buffer is created, continue with setup.
  (when (buffer-live-p gptel-buffer)
    ;; Within the buffer, start inserting the content.
    (with-current-buffer gptel-buffer
      (insert (concat "* " (projectile-project-name) "\n"))
      (insert (concat "** Files\n"))
      ;; (projectile-project-unignored-files)
      (let ((project-files (projectile-project-files (projectile-project-root))))
        (dolist (file project-files)
          (insert "- " file "\n")))

      ;; After the file list, prepare the buffer for chat input from the user.
      (insert (gptel-prompt-prefix-string))
      (goto-char (point-max)))) ;; Move the cursor to the end of the buffer.

  ;; Finally, display the newly populated buffer to the user.
  (pop-to-buffer gptel-buffer))


(provide 'gptel-project)

;;; gptel-project.el ends here
