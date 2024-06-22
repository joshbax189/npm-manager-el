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
        ;; TODO probably also want to refresh audit data?
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

(defun npm-manager--get-node-modules-path ()
  "Return the path to node_modules active for the current directory."
  (with-temp-buffer
    (shell-command "npm root" 't)
    (string-trim (buffer-string))))

(defun npm-manager-parse-package-json ()
  "Store parsed package.json in buffer-local variable."
  (setq npm-manager-package-json (json-read-file (npm-manager--get-package-json-path))))

(defvar npm-manager-audit-json nil "Parsed output of npm audit.")
(make-variable-buffer-local 'npm-manager-audit-json)

(aio-defun npm-manager-run-package-audit (manager-buffer)
  "Store output of npm audit in buffer-local variable of MANAGER-BUFFER."
  (if (not (file-exists-p (npm-manager--get-node-modules-path)))
      (message "Skipping npm audit")
    ;; else
    (let ((audit-json))
      ;; aio-await macro doesn't like to be in the let?
      (setq audit-json (car (aio-await (npm-manager--capture-command "npm audit --json"))))
      (with-current-buffer manager-buffer
        (setq npm-manager-audit-json audit-json)
        (message "completed package audit")
        (tablist-revert)))))

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
         (proc-buff (get-buffer-create (format "npm-manager-proc %s" command-string))))
    (prog1
        promise
      (with-current-buffer proc-buff
        (erase-buffer))
      (make-process
       :name "npm-manager-proc"
       :buffer (format "npm-manager-proc %s" command-string)
       :command (string-split command-string)
       :noquery 't
       :sentinel (lambda (proc string)
                   (cond
                    ((equal string "run\n") nil)
                    ((equal string "finished\n")
                     (with-current-buffer (process-buffer proc)
                       (beginning-of-buffer)
                       ;; note json-parse-buffer does not seem to handle trailing whitespace
                       ;; json-parse-string seems more robust

                       (apply callback (json-parse-string (buffer-string) :object-type 'alist) nil)))
                    ;; TODO kill the process buffer after parsing
                    ((string-prefix-p "exited abnormally" string)
                     ;; some npm commands give non-zero exit code AND produce the output we want!
                     (condition-case nil
                         (with-current-buffer (process-buffer proc)
                           (beginning-of-buffer)
                           (when-let ((buffer-json (json-parse-string (buffer-string) :object-type 'alist)))
                             (apply callback buffer-json nil)))
                       (error
                        (message "npm process %s" string)
                        (message "see buffer %s" (process-buffer proc))
                        (aio-cancel promise "Node exited"))))
                    ('t (message string))))))))

(defun npm-manager--display-command (command flags args &optional dir)
  "Run a command and display output in a new buffer.

Command will be like 'npm COMMAND FLAGS ARGS' where:
  COMMAND is a string
  FLAGS is a string, and
  ARGS is a string.

Returns an aio-promise that is fulfilled with the output buffer."
  (-let (((callback . promise) (aio-make-callback :once 't))
         (proc-buffer (get-buffer-create "*NPM output*")))
    (prog1
        promise
      (with-current-buffer proc-buffer
        (erase-buffer)
        (when dir (setq default-directory dir)))
      (make-process
       :name "npm-manager-proc"
       :buffer proc-buffer
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

(defun npm-manager--make-entry (dependencies package-name)
  "Create tablist entry given PACKAGE-NAME symbol.

DEPENDENCIES is the output of npm list --json."
  (-let* (((dependency requested-version) (npm-manager-read-dep-type package-name))
          (name (symbol-name package-name))
          (propertized-name (if (equal dependency "req")
                                (propertize name 'font-lock-face 'bold)
                              name))
          (installed-version (map-nested-elt dependencies (list package-name 'version)))
          (vulnerabilities (npm-manager-read-vuln package-name)))

   (apply #'vector
          (list
           propertized-name
           dependency
           requested-version
           (or installed-version "-")
           vulnerabilities))))

(defun npm-manager-refresh ()
  "Refresh the contents of NPM manager display."
  (interactive)
  (unless npm-manager-package-json (npm-manager-parse-package-json))
  ;; trigger async call but don't block
  (unless npm-manager-audit-json (npm-manager-run-package-audit (current-buffer)))

  (message (map-elt npm-manager-package-json 'name))
  (message (map-elt npm-manager-package-json 'version))
  (let* ((installed-packages (npm-manager-list-installed-versions))
         (package-names (if installed-packages
                            (map-keys installed-packages)
                          (map-keys (npm-manager-read-packages)))))
    (--map (list it (npm-manager--make-entry installed-packages it))
           package-names)))

(defun npm-manager-list-installed-versions ()
  "Return the dependencies prop of npm list.  This is a list of installed dependency versions."
  (condition-case nil
      (let* ((output (aio-wait-for (npm-manager--capture-command "npm list --json")))
             (all-dependencies (map-elt (car output) 'dependencies))
             ;; remove dependencies marked "extraneous"
             (filtered-dependencies (--filter
                                     (not (map-nested-elt all-dependencies `(,it extraneous)))
                                     all-dependencies)))
        filtered-dependencies)
    (error '())))

(defun npm-manager-read-dep-type (package-name)
  "Look up dependency type (dev, peer, etc) of symbol PACKAGE-NAME.
Returns a list: (type requested-version)."
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

(defun npm-manager-read-packages ()
  "Use to directly list packages from package.json as a backup to npm-manager-list-installed-versions."
  (unless npm-manager-package-json (error "Missing package.json"))

  (append
   (map-elt npm-manager-package-json 'dependencies) ;; list of cons cells (package-symbol . "version")
   (map-elt npm-manager-package-json 'devDependencies)
   (map-elt npm-manager-package-json 'peerDependencies)
   (map-elt npm-manager-package-json 'optionalDependencies)
   (map-elt npm-manager-package-json 'bundleDependencies)))

(defun npm-manager-read-vuln (package-name)
  "Get vulnerability reports for PACKAGE-NAME symbol.
Returns a string high/medium/low or empty."
  (let ((severity (map-nested-elt npm-manager-audit-json `(vulnerabilities ,package-name severity) "")))
    (cond
     ((equal severity "high") (propertize severity 'font-lock-face 'error))
     ((equal severity "medium") (propertize severity 'font-lock-face 'warning))
     ('t severity))))

(aio-defun npm-manager-uninstall ()
  "Uninstall package at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (name (seq-elt entry 0))
         (npm-buffer (current-buffer)))
    (aio-await (npm-manager--display-command "uninstall" "" name))
    (with-current-buffer npm-buffer (revert-buffer))))

(aio-defun npm-manager-install-types-package (base-package-name)
  "Install the types package for BASE-PACKAGE-NAME."
  (interactive (completing-read
                "@types/"
                (list (or (string-remove-prefix "@types/" (seq-elt (tabulated-list-get-entry) 0))
                          ""))))
  (let ((npm-buffer (current-buffer)))
    (aio-await (npm-manager--display-command "i" "-D" (format "@types/%s" base-package-name) default-directory))
    (with-current-buffer npm-buffer
      (when (equal major-mode 'npm-manager-mode) (revert-buffer)))))

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
