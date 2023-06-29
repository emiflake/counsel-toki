;;; counsel-toki.el ---  counsel support for toki pona dictionary lookup. -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Lookup toki pona words within Emacs!
;;
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
      (counsel-toki--insert-section "Definition"
				    (alist-get 'en (alist-get 'def word-data)))
      (counsel-toki--insert-section "Etymology"
				    (alist-get 'etymology word-data))
      (counsel-toki--insert-section "Commentary"
				    (alist-get 'commentary word-data))
      ;; We check here because not all users may have org-mode available, so we are a bit lenient.
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
