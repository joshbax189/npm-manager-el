;;; npm-manager.el --- Emacs list and manage npm installation   -*- lexical-binding: t -*-

;; Copyright (C) 2024 Josh Bax

;; Author: Josh Bax

;; Keywords: languages
;; URL: https://github.com/joshbax189/npm-manager-el

;; TODO which versions?
;; Package-Requires: (aio dash json tablist transient)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Display and interact with the contents of package.json.

;;; Code:

(require 'aio)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)
(require 'ansi-color)
(require 'filenotify)

(defvar npm-manager-package-json nil "Parsed package json.")
(make-variable-buffer-local 'npm-manager-package-json)

(defvar npm-manager-package-json-watcher nil "Descriptor for package.json watcher.")
(make-variable-buffer-local 'npm-manager-package-json-watcher)

(defun npm-manager--change-handler (event)
  "Handle change in package.json.

Filenotify callback responding to change EVENT."
  (let ((event-type (nth 1 event))
        (file (nth 2 event)))

    (when (equal 'changed event-type)
        (message "package.json change detected")
     ;; TODO probably want to refresh manager view, but not working
        ;; TODO probably also want to refresh audit data, but that is longer running
        (npm-manager-parse-package-json))))

(defun npm-manager--set-package-watch (manager-buffer)
  "Reload MANAGER-BUFFER's package.json file on file change."
  (with-current-buffer manager-buffer
    (unless npm-manager-package-json-watcher
      (let ((package-json (npm-manager--get-package-json-path))
            (cb (lambda (event) (with-current-buffer manager-buffer (npm-manager--change-handler event)))))
        (setq npm-manager-package-json-watcher
              (file-notify-add-watch package-json '(change) cb))))))

(defun npm-manager--remove-package-watch ()
  "Remove file watcher from package.json.

Must be called with the npm-manager buffer as current."
  (when npm-manager-package-json-watcher
    (message "removed package.json watcher")
    (file-notify-rm-watch npm-manager-package-json-watcher)))

(defun npm-manager--get-package-json-path ()
  "Return the path to package.json active for the current directory."
  (let ((prefix (with-temp-buffer
                  (shell-command "npm prefix" 't)
                  (string-trim (buffer-string)))))
    (concat prefix "/package.json")))

(defun npm-manager-parse-package-json ()
  "Store parsed package.json in buffer-local variable."
  (setq npm-manager-package-json (json-read-file (npm-manager--get-package-json-path))))

(defvar npm-manager-audit-json nil "Parsed output of npm audit.")
(make-variable-buffer-local 'npm-manager-audit-json)

(defun npm-manager-run-package-audit ()
  "Store output of npm audit in buffer-local variable."
  (let ((tmp nil))
  (with-temp-buffer
    ;; TODO errors when there is no lockfile
    (shell-command "npm audit --json" 't)
    (beginning-of-buffer)
    (setq tmp (json-parse-buffer :object-type 'alist)))
  (setq npm-manager-audit-json tmp)))

(defun npm-manager-info ()
  "Run `npm info` on the package at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (name (seq-elt entry 0))
         (ver (seq-elt entry 2)))
    ;; (async-shell-command (format "npm info %s@%s" name ver) "*NPM output*")
    (npm-manager--display-command "info" "" (format "%s@%s" name ver))))

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
       :command (string-split command-string)
       :noquery 't
       :sentinel (lambda (proc string)
                   (cond
                    ((equal string "run\n") nil)
                    ((equal string "finished\n")
                     (with-current-buffer (process-buffer proc)
                       (beginning-of-buffer)
                       (apply callback (json-parse-buffer :object-type 'alist) nil)))
                    ;; TODO kill the process buffer after parsing
                    ('t (message string))))))))

(defun npm-manager--display-command (command flags args)
  "Run a command and display output in a new buffer.

Command will be like 'npm COMMAND FLAGS ARGS' where:
  COMMAND is a string
  FLAGS is a string, and
  ARGS is a string.

Returns an aio-promise that is fulfilled with the output buffer."
  (when (get-buffer "*NPM output*") (kill-buffer "*NPM output*")) ;; TODO do we need this?
  (-let (((callback . promise) (aio-make-callback :once 't)))
    (prog1
        promise
        (make-process
         :name "npm-manager-proc"
         :buffer (get-buffer-create "*NPM output*")
         :command (append (list "npm" command) (string-split flags) '("--color" "always") (string-split args))
         :noquery 't
         :filter (lambda (proc string)
                   (when (buffer-live-p (process-buffer proc))
                     (with-current-buffer (process-buffer proc)
                       (let ((moving (= (point) (process-mark proc))))
                         (save-excursion
                           ;; Insert the text, advancing the process marker.
                           (goto-char (process-mark proc))
                           (insert (ansi-color-apply string))
                           (set-marker (process-mark proc) (point)))
                         (if moving (goto-char (process-mark proc)))))))
         :sentinel (lambda (proc string)
                     (cond
                      ((equal string "run\n") nil)
                      ((equal string "finished\n")
                       (with-current-buffer (process-buffer proc)
                         (shell-mode)
                         (view-mode)
                         (pop-to-buffer (current-buffer))
                         (apply callback (current-buffer) nil)))
                      ;; TODO handle errors, ensure promise works correctly
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
    (message (alist-get 'name data))
    (message (alist-get 'version data))
    (let* ((deps (alist-get 'dependencies data))
           (dep-keys (--filter
                      (not (map-nested-elt deps `(,it extraneous)))
                      (map-keys deps))))
      (--map (list it
                   (apply 'vector `(,(symbol-name it) ,@(npm-manager-read-dep-type it) ,(map-nested-elt deps (list it 'version)) ,(npm-manager-read-vuln it))))
             dep-keys))))

(defun npm-manager-read-dep-type (package-name)
  "Look up dependency type (dev, peer, etc) of symbol PACKAGE-NAME."
(let ((core-deps     (map-nested-elt npm-manager-package-json `(dependencies ,package-name)))
      (dev-deps      (map-nested-elt npm-manager-package-json `(devDependencies ,package-name)))
      (peer-deps     (map-nested-elt npm-manager-package-json `(peerDependencies ,package-name)))
      (optional-deps (map-nested-elt npm-manager-package-json `(optionalDependencies ,package-name)))
      (bundle-deps   (map-nested-elt npm-manager-package-json `(bundleDependencies ,package-name))))

  (or
   (and core-deps     `("req" ,core-deps))
   (and dev-deps      `("dev" ,dev-deps))
   (and peer-deps     `("peer" ,peer-deps))
   (and optional-deps `("opt" ,optional-deps))
   (and bundle-deps   `("bundle" ,bundle-deps))
   '("" ""))))

(defun npm-manager-read-vuln (package-name)
  "docstring"
  (map-nested-elt npm-manager-audit-json `(vulnerabilities ,package-name severity) ""))

(aio-defun npm-manager-uninstall ()
  "Uninstall package at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (name (seq-elt entry 0))
         (npm-buffer (current-buffer)))
    (aio-await (npm-manager--display-command "uninstall" "" name))
    (with-current-buffer npm-buffer (revert-buffer))))

;;;###autoload
(defun npm-manager ()
  "Start npm manager interface in the directory."
  (interactive)
  (pop-to-buffer (format "NPM %s" default-directory))
  (npm-manager-parse-package-json)
  (npm-manager-mode)
  (npm-manager--set-package-watch (current-buffer))
  (add-hook 'kill-buffer-hook #'npm-manager--remove-package-watch)
  (tablist-revert))

(defun npm-manager-unload-function ()
  "Cleanup mode hooks."
  (remove-hook 'kill-buffer-hook #'npm-manager--remove-package-watch))

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
