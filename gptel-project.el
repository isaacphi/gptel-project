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

;; Define a new function called 'my-gptel-setup-with-files'.
(defun my-gptel-setup-with-files ()
  "Setup a GPTel buffer with a list of project files."
  ;; Mark this function for interactive use, enabling it to be called with M-x.
  (interactive)
  ;; Check if Projectile is loaded (featurep 'projectile)
  ;; and if the current buffer is in a Projectile project (projectile-project-p).
  (unless (and (featurep 'projectile) (projectile-project-p))
    ;; If not, raise an error to inform the user.
    (error "Projectile is not available, or you're not in a Projectile project"))

  ;; Create and configure a new buffer specifically for GPTel.
  (let ((gptel-buffer (get-buffer-create "*GPTel-Project-Files*")))
    (with-current-buffer gptel-buffer
      (org-mode)   ;; Enable Org mode, which is commonly used for structured notes.
      (gptel-mode) ;; Enable the hypothetical 'gptel-mode'.

    ;; Within the buffer, start inserting the content.
    (let ((project-files (projectile-project-files (projectile-project-root))))
      ;; Insert the project files, one on each line prefixed with "- ".
      (with-current-buffer gptel-buffer
        (dolist (file project-files) ;; Loop over the list of project files.
          (insert "- " file "\n")))) ;; Insert each file followed by a newline.

    ;; After the file list, prepare the buffer for chat input from the user.
    (with-current-buffer gptel-buffer
      (insert "\n\n* Your chat input here\n") ;; Add a prompt for the user's input.
      (goto-char (point-max))) ;; Move the cursor to the end of the buffer.

    ;; Finally, display the newly populated buffer to the user.
    (pop-to-buffer gptel-buffer)))

;;; gptel-project.el ends here
