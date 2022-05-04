;;; darwin.el --- Summary:
;;; Commentary:
;;; Code:

(setq mac-option-modifier 'meta
      mac-command-modifier 'hyper)

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(provide 'darwin)
;;; darwin.el ends here
