;;; counsel-toki.el ---  counsel support for toki pona dictionary lookup. -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Emily Martins

;; Author: Emily Martins <emi@haskell.fyi>
;; Created: 29 Jun 2023
;; URL: https://github.com/emiflake/counsel-toki
;; Package-Version: 0.1.0
;; Package-Requires: ((request "20230127.417") (emacs "24.4") (ivy "0.14.0"))

;; This file is not part of GNU Emacs.

;; This file is licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Lookup toki pona words within Emacs, using ivy.
;; The code word data is requested once upon evaluation and
;; stored in `counsel-toki--data'.

;;; Code:

(require 'request)
(require 'ivy)

(defvar counsel-toki--data
  nil
  "The parsed word data as found on `https://linku.la/jasima/data.json'.")

(request "https://linku.la/jasima/data.json"
  :parser 'json-read
  :success (cl-function
  	    (lambda (&key data &allow-other-keys)
  	      (setq counsel-toki--data data))))

(defun counsel-toki--word-line (word-data)
  "Get the line contents given by WORD-DATA."
  (format "%s - %s" (alist-get 'word word-data)
	  (alist-get 'en (alist-get 'def word-data))))

(defun counsel-toki--function ()
  "Get counsel lines for counsel-toki."
  (mapcar #'counsel-toki--word-line (alist-get 'data counsel-toki--data)))

(defun counsel-toki--insert-section (subtitle value)
  "Insert a section with SUBTITLE, populated with VALUE."
  (when value
    (insert "** " subtitle)
    (newline)
    (newline)
    (insert value)
    (newline)
    (newline)))

(defun counsel-toki--action (word-line)
  "Open a buffer with information about the word in WORD-LINE."
  (interactive)
  (let* ((word (replace-regexp-in-string " -.*" "" word-line))
	 (word-data (alist-get (intern word) (alist-get 'data counsel-toki--data))))
    (with-current-buffer (get-buffer-create word-line)
      (erase-buffer)
      (switch-to-buffer (current-buffer))
      (insert "* " word)
      (newline)
      (newline)
      (insert "- category :: " (alist-get 'usage_category word-data))
      (newline)
      (insert "- book :: " (alist-get 'book word-data))
      (newline)
      (newline)
      (counsel-toki--insert-section "Definition"
				    (alist-get 'en (alist-get 'def word-data)))
      (counsel-toki--insert-section "Etymology"
				    (alist-get 'etymology word-data))
      (counsel-toki--insert-section "Commentary"
				    (alist-get 'commentary word-data))
      ;; We check here because not all users may have org-mode available, so we are a bit lenient.
      ;; Ultimately the file is readable without org-mode, too.
      (when (fboundp #'org-mode)
	(org-mode)))))

(defun counsel-toki (&optional initial-input)
  "Search for toki pona words with INITIAL-INPUT."
  (interactive)
  (if counsel-toki--data
      (ivy-read "o alasa e nimi: "
		(counsel-toki--function)
		:action #'counsel-toki--action
		:initial-input initial-input)
    (message "counsel-toki has not yet loaded the data. See the variable `counsel-toki--data'.")))

(provide 'counsel-toki)

;;; counsel-toki.el ends here
