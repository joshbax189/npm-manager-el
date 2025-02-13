;; -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'ert-async)
(require 'npm-manager)
(require 'npm-manager-search)
(require 'aio)
(require 'map)

(ert-deftest npm-manager-search--make-search-url/test ()
  "Tests url remains valid after replacement."
  (should (equal (npm-manager-search--make-search-url "foo")
                 "https://registry.npmjs.org/-/v1/search?size=100&text=foo"))
  (should (equal (npm-manager-search--make-search-url "yargs author:bcoe")
                 "https://registry.npmjs.org/-/v1/search?size=100&text=yargs%20author:bcoe"))
  (should (equal (npm-manager-search--make-search-url "@foo/bar")
                 "https://registry.npmjs.org/-/v1/search?size=100&text=@foo/bar")))

;;; npm-manager-search--package-to-entry
(ert-deftest npm-manager-search--package-to-entry/test-defaults ()
  "Tests defaulting behaviour."
  (let* ((input (json-parse-string "{\"downloads\":{},\"dependents\":58739,\"updated\":\"2025-02-11T07:18:51.744Z\",\"searchScore\":1245.1101,\"package\":{\"name\":\"typescript\",\"keywords\":[\"TypeScript\",\"Microsoft\",\"compiler\",\"language\",\"javascript\"],\"maintainers\":[{\"email\":\"typescript@microsoft.com\",\"username\":\"typescript-bot\"},{\"email\":\"wwigham@gmail.com\",\"username\":\"weswigham\"},{\"email\":\"nathan@shively-sanders.com\",\"username\":\"sanders_n\"},{\"email\":\"andrew@wheream.io\",\"username\":\"andrewbranch\"},{\"email\":\"mineyalc@microsoft.com\",\"username\":\"minestarks\"},{\"email\":\"rbuckton@chronicles.org\",\"username\":\"rbuckton\"},{\"email\":\"shkamat@microsoft.com\",\"username\":\"sheetalkamat\"},{\"email\":\"typescript-design@microsoft.com\",\"username\":\"typescript-deploys\"}],\"license\":\"Apache-2.0\",\"date\":\"2025-01-08T19:05:34.766Z\",\"links\":{\"homepage\":\"https://www.typescriptlang.org/\",\"repository\":\"git+https://github.com/microsoft/TypeScript.git\",\"bugs\":\"https://github.com/microsoft/TypeScript/issues\",\"npm\":\"https://www.npmjs.com/package/typescript\"}},\"score\":{\"final\":1245.1101,\"detail\":{\"popularity\":1,\"quality\":1,\"maintenance\":1}},\"flags\":{\"insecure\":0}}"
                                   :object-type 'alist))
         (result (npm-manager-search--package-to-entry input)))
    (should (equal (nth 1 result)
                   (vector "typescript"
                           ""
                           ""
                           "2025-01-08"
                           ""
                           "0")))))

(ert-deftest npm-manager-search--package-to-entry/test-basic ()
  "Tests normal behaviour."
  (let* ((input (json-parse-string "{\"downloads\":{\"monthly\":282204779,\"weekly\":70859588},\"dependents\":58739,\"updated\":\"2025-02-11T07:18:51.744Z\",\"searchScore\":1245.1101,\"package\":{\"name\":\"typescript\",\"keywords\":[\"TypeScript\",\"Microsoft\",\"compiler\",\"language\",\"javascript\"],\"version\":\"5.7.3\",\"description\":\"TypeScript is a language for application scale JavaScript development\",\"publisher\":{\"email\":\"typescript@microsoft.com\",\"username\":\"typescript-bot\"},\"maintainers\":[{\"email\":\"typescript@microsoft.com\",\"username\":\"typescript-bot\"},{\"email\":\"wwigham@gmail.com\",\"username\":\"weswigham\"},{\"email\":\"nathan@shively-sanders.com\",\"username\":\"sanders_n\"},{\"email\":\"andrew@wheream.io\",\"username\":\"andrewbranch\"},{\"email\":\"mineyalc@microsoft.com\",\"username\":\"minestarks\"},{\"email\":\"rbuckton@chronicles.org\",\"username\":\"rbuckton\"},{\"email\":\"shkamat@microsoft.com\",\"username\":\"sheetalkamat\"},{\"email\":\"typescript-design@microsoft.com\",\"username\":\"typescript-deploys\"}],\"license\":\"Apache-2.0\",\"date\":\"2025-01-08T19:05:34.766Z\",\"links\":{\"homepage\":\"https://www.typescriptlang.org/\",\"repository\":\"git+https://github.com/microsoft/TypeScript.git\",\"bugs\":\"https://github.com/microsoft/TypeScript/issues\",\"npm\":\"https://www.npmjs.com/package/typescript\"}},\"score\":{\"final\":1245.1101,\"detail\":{\"popularity\":1,\"quality\":1,\"maintenance\":1}},\"flags\":{\"insecure\":0}}"
                                   :object-type 'alist))
         (result (npm-manager-search--package-to-entry input)))
    (should (equal (nth 1 result)
                   (vector "typescript"
                           "TypeScript is a language for application scale JavaScript development"
                           "typescript-bot"
                           "2025-01-08"
                           "5.7.3"
                           "71M")))))

;; npm-manager-search
;; transient => npm-manager-search--search-suffix
;; helpers: npm-manager-search-text, npm-manager-search--fetch, via refresh

;; npm-manager-search-info
;; TODO this would be improved by a projection function on the tablist entry
;; otherwise just dispatches to display-command

;; npm-manager-search-install
;; transient => npm-manager-search-install-suffix
;; dispatches to display-command

;;; npm-manager-search--fetch
(ert-deftest-async npm-manager-search--fetch/test-basic (done)
  "normal case: contents of url-retrieve buffer is turned into json, returned as a promise"
  (with-mock
    (mock (aio-url-retrieve *) => (let ((p (aio-promise)))
                                    (prog1 p
                                      (aio-resolve p (lambda () (with-current-buffer (get-buffer-create (make-temp-name "fake-http"))
                                                                  (insert "HTTP/1.1 200 OK
    Date: Tue, 11 Feb 2025 20:22:57 GMT
    Content-Type: application/json

    {\"objects\":[{\"downloads\":{\"monthly\":282204779,\"weekly\":70859588},\"dependents\":58739,\"updated\":\"2025-02-11T07:18:51.744Z\",\"searchScore\":1245.1101,\"package\":{\"name\":\"typescript\",\"keywords\":[\"TypeScript\",\"Microsoft\",\"compiler\",\"language\",\"javascript\"],\"version\":\"5.7.3\",\"description\":\"TypeScript is a language for application scale JavaScript development\",\"publisher\":{\"email\":\"typescript@microsoft.com\",\"username\":\"typescript-bot\"},\"maintainers\":[{\"email\":\"typescript@microsoft.com\",\"username\":\"typescript-bot\"},{\"email\":\"wwigham@gmail.com\",\"username\":\"weswigham\"},{\"email\":\"nathan@shively-sanders.com\",\"username\":\"sanders_n\"},{\"email\":\"andrew@wheream.io\",\"username\":\"andrewbranch\"},{\"email\":\"mineyalc@microsoft.com\",\"username\":\"minestarks\"},{\"email\":\"rbuckton@chronicles.org\",\"username\":\"rbuckton\"},{\"email\":\"shkamat@microsoft.com\",\"username\":\"sheetalkamat\"},{\"email\":\"typescript-design@microsoft.com\",\"username\":\"typescript-deploys\"}],\"license\":\"Apache-2.0\",\"date\":\"2025-01-08T19:05:34.766Z\",\"links\":{\"homepage\":\"https://www.typescriptlang.org/\",\"repository\":\"git+https://github.com/microsoft/TypeScript.git\",\"bugs\":\"https://github.com/microsoft/TypeScript/issues\",\"npm\":\"https://www.npmjs.com/package/typescript\"}},\"score\":{\"final\":1245.1101,\"detail\":{\"popularity\":1,\"quality\":1,\"maintenance\":1}},\"flags\":{\"insecure\":0}}]}")
                                                                  (cons nil (current-buffer))))))))
    (let ((test-result (aio-wait-for (npm-manager-search--fetch "foo"))))
      (should (map-elt test-result 'objects))
      (funcall done))))

(ert-deftest-async npm-manager-search--fetch/test-utf8 (done)
  "non utf8 chars can cause json parser to error."
  (with-mock
    (mock (aio-url-retrieve *) => (let ((p (aio-promise)))
                                    (prog1 p
                                      (aio-resolve p (lambda () (with-current-buffer (get-buffer-create (make-temp-name "fake-http"))
                                                                  (insert "HTTP/1.1 200 OK
    Date: Tue, 11 Feb 2025 20:22:57 GMT
    Content-Type: application/json

    {\"objects\":[{\"description\":\"🅿️ Reusable My Prettier Config\"}]}")
                                                                  (cons nil (current-buffer))))))))
    (let ((test-result (aio-wait-for (npm-manager-search--fetch "foo"))))
      (should (map-elt test-result 'objects))
      (funcall done))))

(ert-deftest-async npm-manager-search--fetch/test-error (done)
  "Check error behaviour."
  (with-mock
    (mock (aio-url-retrieve *) => (let ((p (aio-promise)))
                                    (prog1 p
                                      (aio-with-promise p (cons 'error nil)))))
    (should-error (aio-wait-for (npm-manager-search--fetch "foo")))
    (funcall done)))

;;; npm-manager-search.test.el ends here
