;;; npm-manager.el --- Emacs list and manage npm installation   -*- lexical-binding: t -*-

;;; Code:

;; (require 's)
;; (require 'aio)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)

(defvar npm-manager-package-json nil "Parsed package json.")
(make-variable-buffer-local 'npm-manager-package-json)

(defun npm-manager-parse-package-json ()
  (setq npm-manager-package-json (json-read-file "./package.json")))

(defun npm-manager-refresh ()
  "docstring"
  (interactive)
  (with-temp-buffer
    (shell-command "npm list --json" t)
    (beginning-of-buffer)
    (let ((data (json-parse-buffer)))
      (unless npm-manager-package-json (npm-manager-parse-package-json))
      ;; dependencies
      ;; map over keys- key is package name key.version to print
      (message (gethash "name" data))
      (message (gethash "version" data))
      (let ((deps (gethash "dependencies" data)))
        (--map (list it (seq-into `(,it ,@(npm-manager-read-dep-type it) ,(gethash "version" (gethash it deps)) "") 'vector))
               (hash-table-keys deps))))))

(defun npm-manager-read-dep-type (package-name)
  "docstring"
  (let ((package-sym (intern package-name)))
    ;; TODO switch to case
  (if-let ((package-req (alist-get package-sym (alist-get 'dependencies npm-manager-package-json))))
      `("req" ,package-req)
    (if-let ((package-req (alist-get package-sym (alist-get 'devDependencies npm-manager-package-json))))
        `("dev" ,package-req)
      ;; TODO add peer and optional types
      '("" "")))))

;;;###autoload
(defun npm-manager ()
  "docstring"
  (interactive)
  (pop-to-buffer (format "NPM %s" default-directory))
  (npm-manager-parse-package-json)
  (npm-manager-mode)
  (tablist-revert))

(define-derived-mode npm-manager-mode tabulated-list-mode "NPM Manager"
  "Major mode."
  (setq tabulated-list-format [("Package" 48 t) ("Type" 6 t) ("Requested" 12 t) ("Installed" 12 t) ("Messages" 32 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Type" . t))
  (setq tabulated-list-entries #'npm-manager-refresh)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'npm-manager)

;;; npm-manager.el ends here
