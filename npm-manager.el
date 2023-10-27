;;; npm-manager.el --- Emacs list and manage npm installation   -*- lexical-binding: t -*-

;;; Code:

(require 's)
(require 'aio)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)

(defvar npm-manager-package-json nil "Parsed package json.")
(make-variable-buffer-local 'npm-manager-package-json)

(defun npm-manager-parse-package-json ()
  "Store parsed package.json in buffer-local variable."
  (setq npm-manager-package-json (json-read-file "./package.json")))

;; TODO this is a hash table, should match package-json
(defvar npm-manager-audit-json nil "Parsed output of npm audit.")
(make-variable-buffer-local 'npm-manager-audit-json)

(defun npm-manager-run-package-audit ()
  "Store output of npm audit in buffer-local variable."
  (let ((tmp nil))
  (with-temp-buffer
    (shell-command "npm audit --json" t)
    (beginning-of-buffer)
    (setq tmp (json-parse-buffer)))
  (setq npm-manager-audit-json tmp)))

(defun npm-manager--capture-command (command-string)
  "Run npm command COMMAND-STRING and parse output as JSON, returning it as an aio-promise."
  (-let (((callback . promise) (aio-make-callback :once 't))
         (proc-buff (get-buffer-create (format "npm-manager-proc %s" command-string)
                                       't)))
    (prog1
        promise
      (with-current-buffer proc-buff
        (rename-uniquely))
      (make-process
       :name "npm-manager-proc"
       :buffer proc-buff
       :command (s-split " " command-string)
       :noquery 't
       :sentinel (lambda (proc string)
                   (cond
                    ((equal string "run\n") nil)
                    ((equal string "finished\n")
                     (with-current-buffer (process-buffer proc)
                       (beginning-of-buffer)
                       (apply callback (json-parse-buffer) nil)))
                    ;; TODO kill the process buffer after parsing
                    ('t (message string))))))))

(defun npm-manager-refresh ()
  "Refresh the contents of NPM manager display."
  (interactive)
  (unless npm-manager-package-json (npm-manager-parse-package-json))
  (unless npm-manager-audit-json (npm-manager-run-package-audit))

  ;; TODO not sure why this gets a list?
  (let ((data (car (aio-wait-for (npm-manager--capture-command "npm list --json")))))
    ;; dependencies
    ;; map over keys- key is package name key.version to print
    (message (gethash "name" data))
    (message (gethash "version" data))
    (let* ((deps (gethash "dependencies" data))
           (dep-keys (--filter
                      (not (gethash "extraneous" (gethash it deps)))
                      (hash-table-keys deps))))
      (--map (list it
                   (apply 'vector `(,it ,@(npm-manager-read-dep-type it) ,(gethash "version" (gethash it deps)) ,(npm-manager-read-vuln it))))
             dep-keys))))

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

(defun npm-manager-read-vuln (package-name)
  "docstring"
  (let ((vulns (gethash "vulnerabilities" npm-manager-audit-json)))
    (if-let ((package-vuln (gethash package-name vulns)))
        (gethash "severity" package-vuln)
      ""
  )))

;;;###autoload
(defun npm-manager ()
  "Start npm manager interface in the directory."
  (interactive)
  (pop-to-buffer (format "NPM %s" default-directory))
  (npm-manager-parse-package-json)
  (npm-manager-mode)
  (tablist-revert))

(define-derived-mode npm-manager-mode tabulated-list-mode "NPM Manager"
  "NPM manager major mode."
  (setq tabulated-list-format [("Package" 48 t)
                               ("Type" 6 t)
                               ("Requested" 12 t)
                               ("Installed" 12 t)
                               ("Messages" 32 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Type" . t)
        tabulated-list-entries #'npm-manager-refresh)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'npm-manager)

;;; npm-manager.el ends here
