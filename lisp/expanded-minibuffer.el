;;; expanded-minibuffer --- Summary:
;;; Commentary:
;;; Code:

(defun expanded-minibuffer-mode-exit (&optional run-on-exit)
  "Extract the current string of the buffer and input it into the minibuffer.
If RUN-ON-EXIT is specified, minibuffer is exited and run with the contents."
  (interactive)
  (let ((new-contents (buffer-string)))
    (kill-buffer-and-window)
    (switch-to-minibuffer)
    (delete-minibuffer-contents)
    (insert new-contents)
    (when run-on-exit
      (read--expression-try-read))))

(defun expanded-minibuffer-mode-exit&run ()
  "Extract the current string of the buffer and input it into the minibuffer.
Then run it."
  (interactive)
  (expanded-minibuffer-mode-exit t))

(defun expanded-minibuffer-mode-abort (&optional abort-minibuffer)
  "Abort expanded-minibuffer and if ABORT-MINIBUFFER, minibuffer as well."
  (interactive)
  (kill-buffer-and-window)
  (switch-to-minibuffer)
  (when abort-minibuffer
    (abort-minibuffers)))

(defun expanded-minibuffer-mode-abort&exit ()
  "Abort expanded-minibuffer and minibuffer as well."
  (interactive)
  (expanded-minibuffer-mode-abort t))

(defun expanded-minibuffer-mode-enter ()
  "Expand the current minibuffer with CONTENTS."
  (interactive)
  (let ((expanded-minibuffer-mode-buffer (get-buffer-create "*expanded-minibuffer*"))
        (contents (minibuffer-contents)))
    (switch-to-buffer-other-window expanded-minibuffer-mode-buffer)
    (expanded-minibuffer-mode)
    (setq-local header-line-format
	        (substitute-command-keys
	         "Edit, then exit with `\\[expanded-minibuffer-mode-exit]'/`\\[expanded-minibuffer-mode-exit&run]', or abort \
with `\\[expanded-minibuffer-mode-abort]'/`\\[expanded-minibuffer-mode-abort&exit]'"))
    (insert contents)))

(define-derived-mode expanded-minibuffer-mode
  lisp-interaction-mode "ExpandedMinibuf"
  "Expand the current minibuffer with the text currently in it.
Puts it in a new window for easier editing, and upon exit the new string
will be put into the original minibuffer."
  :group '(convenience minibuffer)
  (define-key expanded-minibuffer-mode-map (kbd "C-c '") #'expanded-minibuffer-mode-exit)
  (define-key expanded-minibuffer-mode-map (kbd "C-c C-'") #'expanded-minibuffer-mode-exit&run)
  (define-key expanded-minibuffer-mode-map (kbd "C-c k") #'expanded-minibuffer-mode-abort)
  (define-key expanded-minibuffer-mode-map (kbd "C-c C-k") #'expanded-minibuffer-mode-abort&exit))

(provide 'expanded-minibuffer)
;;; expanded-minibuffer.el ends here
