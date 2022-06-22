;;; tab-modeline-mode --- Summary
;;; Commentary:
;;; Code:

(define-minor-mode tab-modeline-mode
  "Show name of current tab in mode line."
  :group 'convenience
  :global t
  :lighter (:eval (format " TAB[%s (%s/%s)]"
                          (cdr (assoc 'name (tab-bar--current-tab)))
                          (1+ (tab-bar--current-tab-index))
                          (length (tab-bar-tabs))))
  (if tab-modeline-mode
      (progn
        (setq tab-bar-new-tab-choice "*scratch*"
              tab-bar-show nil)
        (tab-bar-mode))
    (tab-bar-mode -1)))

(provide 'tab-modeline)
;;; tab-modeline.el ends here
