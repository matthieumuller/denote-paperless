;;; denote-paperless.el --- Simple document management with an efficient file-naming scheme -*- lexical-binding: t -*-

;; Copyright (C) 2026  Matthieu MULLER

;; Author: Matthieu MULLER <contact@mmuller.dev>
;; Maintainer: Matthieu MULLER <contact@mmuller.dev>
;; URL: https://github.com/matthieumuller/denote-paperless
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (denote "4.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Similarly to Denote, Denote Paperless aims to be a simple-to-use,
;; focused-in-scope, and an effective document management tool for Emacs.
;;
;; "Files should follow a predictable and descriptive file-naming scheme."
;;
;; This package follows the same idea than its parent package, Denote, but
;; applies its own logic on top of it to easily rename and manage documents.
;; Denote Paperless is essentially an extension of Denote that works like a
;; trimmed-down version of Paperless-ngx.
;;
;; It builds on the curated Denote's code base to bring 4 new subcomponents: a
;; document date, a document type, a correspondent, and a archive serial number.
;; Read the manual to understand how they integrate to Denote's logic and how
;; you should use them: https://github.com/matthieumuller/denote-paperless
;;
;; Denote Paperless also tries its best to embrace the code design principles of
;; Denote:
;;
;; * Predictability :: File names must follow a consistent and descriptive
;;   naming convention.
;; 
;; * Composability :: Integrate with other packages or built-in functionality
;;   instead of re-inventing the wheel (Unix phylosophy).
;;
;; * Portability :: PDF documents and images aren't plain text, but we can do
;;   our best to mitigate the use of, yet another, binary-based system.  No need
;;   for a database or some specialized software.
;;
;; * Flexibility :: Do not assume the user's preference.  This package will not
;;   configure variables for you, it is up to the user to decide.  Configuration
;;   examples are provided in the manual.
;;
;; * Hackability :: Denote Paperless's code base consists of small and reusable
;;   functions that all have documentation strings (similarly to Denote's code
;;   base).  Feel free to modify and tweak the workflow to your liking.

;;; Code:

(require 'denote)

(defgroup denote-paperless ()
  "Paperless system on top of `denote', an efficient file-naming scheme for your documents."
  :group 'files
  :link '(info-link "(denote-paperless) Top")
  :link '(url-link :tag "Homepage" "https://github.com/matthieumuller/denote-paperless"))

(defun denote-paperless-string-to-integer (string)
  "Parse STRING as an integer number and return the number.

Much more strict than `string-to-number'.  It won't ignore non-integer
characters.  Return nil if STRING cannot be parsed as a integer."
  (when (and (stringp string) (string-match-p "^[0-9]+$" string))
    (string-to-number string)))

(defun denote-paperless-read-lines-to-list (filepath)
  "Return a list of strings based on the content of the file at FILEPATH.

Each element of the list is a line from the content of the file.  Empty lines
are ignored."
  (if filepath
      (with-temp-buffer
        (insert-file-contents filepath)
        (flush-lines "^[[:space:]]*$") ; Remove empty lines
        (split-string (buffer-string) "\n" t))
    (list)))

(defun denote-paperless-append-to-file (filepath string &optional allow-duplicate)
  "Append the string STRING to the end of the file at FILEPATH.

If ALLOW-DUPLICATE is nil and STRING already exists in the file then the append
operation is aborted."
  (let* ((elements (denote-paperless-read-lines-to-list filepath))
         (is-in-file (member string elements)))
    (if (or (not is-in-file) (and is-in-file allow-duplicate))
        (with-temp-file filepath
          (insert-file-contents filepath)
          (end-of-buffer)
          (insert (concat string "\n"))) 
      (message "Element '%s' already exists in file '%s'" string filepath))))

(defcustom denote-paperless-known-correspondents-file
  (expand-file-name "~/Documents/paperless/correspondents.txt")
  "Path of file containing the correspondents.")

(defcustom denote-paperless-known-doctypes-file
  (expand-file-name "~/Documents/paperless/doctypes.txt")
  "Path of file containing the document types.")

(defvar denote-paperless-known-correspondents '()
  "List of correspondents")

(defvar denote-paperless-known-doctypes '()
  "List of document types")

(defun denote-paperless-refresh-correspondents ()
  "Repopulate the `denote-paperless-known-correspondents' variable based on
`denote-paperless-known-correspondents-file'."
  (interactive)
  (setq denote-paperless-known-correspondents
        (denote-paperless-read-lines-to-list denote-paperless-known-correspondents-file)))

(defun denote-paperless-refresh-doctypes ()
  "Repopulate the `denote-paperless-known-doctypes' variable based on
`denote-paperless-known-doctypes-file'."
  (interactive)
  (setq denote-paperless-known-doctypes
        (denote-paperless-read-lines-to-list denote-paperless-known-doctypes-file)))

(defun denote-paperless-add-correspondent (name)
  "Convienience function to add a CORRESPONDENT named NAME to
`denote-paperless-known-correspondents-file'"
  (interactive "sNew correspondent: ")
  (denote-paperless-append-to-file denote-paperless-known-correspondents-file
                                      name)
  (denote-paperless-refresh-correspondents))

(defun denote-paperless-add-doctype (name)
  "Convienience function to add a DOCTYPE named NAME to
`denote-paperless-known-doctypes-file'"
  (interactive "sNew document type: ")
  (denote-paperless-append-to-file denote-paperless-known-doctypes-file
                                      name)
  (denote-paperless-refresh-doctypes))

(defconst denote-paperless-asn-regexp "##\\([^.]*?\\)\\(%%.*\\|&&.*\\)*$"
  "Regular expression to match the ASN (Archive Serial Number) field in a SIGNATURE.")

(defconst denote-paperless-docdate-regexp "^\\([0-9]\\{8\\}\\)"
  "Regular expression to match the DOCDATE field in a SIGNATURE.")

(defconst denote-paperless-doctype-regexp "&&\\([^.]*?\\)\\(%%.*\\|##.*\\)*$"
  "Regular expression to match the DOCTYPE field in a SIGNATURE.")

(defconst denote-paperless-correspondent-regexp "%%\\([^.]*?\\)\\(##.*\\|&&.*\\)*$"
  "Regular expression to match the CORRESPONDENT field in a SIGNATURE.")

(defun denote-paperless-retrieve-asn (component)
  "Extract ASN from COMPONENT, if present, else return nil.

Based on `denote-retrieve-filename-*' functions."
  (when (and component
             (string-match denote-paperless-asn-regexp component))
    (match-string 1 component)))

(defun denote-paperless-retrieve-docdate (component)
  "Extract docdate from COMPONENT, if present, else return nil.
Returns an encoded date (lisp timestamp).

Based on `denote-retrieve-filename-*' functions."
  (when (and component
             (string-match denote-paperless-docdate-regexp component))
    (encode-time (decoded-time-add (parse-time-string (match-string 0 component))
                                   (make-decoded-time :second 0)))))

(defun denote-paperless-retrieve-doctype (component)
  "Extract doctype from COMPONENT, if present, else return nil.

Based on `denote-retrieve-filename-*' functions."
  (when (and component
             (string-match denote-paperless-doctype-regexp component))
    (match-string 1 component)))

(defun denote-paperless-retrieve-correspondent (component)
  "Extract correspondent from COMPONENT, if present, else return nil.

Based on `denote-retrieve-filename-*' functions."
  (when (and component
             (string-match denote-paperless-correspondent-regexp component))
    (match-string 1 component)))

(defun denote-paperless-retrieve-max-asn ()
  "Find the maximum ASN value amongst all denote files.

Print the maximum ASN when called interactively, else return the maximum ASN."
  (interactive)
  (let* ((files (denote-directory-files))
         (signatures (delq nil (mapcar #'denote-retrieve-filename-signature files)))
         (asns (delq nil
                     (mapcar #'denote-paperless-string-to-integer
                             (mapcar #'denote-paperless-retrieve-asn
                                     signatures))))
         (max-asn (if asns (apply #'max asns) 0)))
    (if (called-interactively-p)
        (message "Current maximum ASN: %s" max-asn)
        max-asn)))

(defun denote-paperless-asn-prompt (&optional initial-asn prompt-text)
  "Prompt for an ASN (Archive Serial Number) string.

With optional INITIAL-ASN use it as the initial minibuffer text.
With optional PROMPT-TEXT use it in the minibuffer instead of the default prompt.

Always returns a string."
  (when (and initial-asn (string-empty-p initial-asn))
    (setq initial-asn nil))
  (let* ((max-asn (denote-paperless-retrieve-max-asn))
         (asn (if initial-asn
                  initial-asn
                (number-to-string (1+ max-asn))))
         (prompt (concat "ASN "
                         (or prompt-text "for the new file")
                         (format " [current maximum is %s]" (number-to-string max-asn)))))
    (denote--with-conditional-completion
     'denote-paperless-asn-prompt
     (format-prompt prompt nil)
     nil
     initial-asn)))

(defun denote-paperless-valid-date-p (date)
  "Return DATE as a valid date.

A valid DATE is a value that can be parsed by either `decode-time' or
`parse-time-string'.  DATE must be an encoded date or a string.
If DATE is nil or an empty string, return nil.

Inpiration from `denote-paperless-valid-date-p'."
  (cond ((or (null date) (and (stringp date) (string-empty-p date)))
         nil)
        ((and (or (numberp date) (listp date))
              (decode-time date))
         ;; Already encoded date (as list or number)
         date)
        (t
         ;; Non-empty strings (e.g. "2024-01-01", "2024-01-01 12:00", etc.)
         (encode-time (decoded-time-add (parse-time-string date)
                                        (make-decoded-time :day 0))))))

(defun denote-paperless-docdate-prompt (&optional initial-docdate prompt-text)
  "Prompt for a document date.

With optional INITIAL-DOCDATE use it as the initial minibuffer text.
With optional PROMPT-TEXT use it in the minibuffer instead of the default prompt.

Always returns a string (e.g. '2022-06-16 14:30').

Inspiration from `denote-date-prompt'."
  (let* ((encoded-docdate (denote-paperless-valid-date-p initial-docdate))
         (initial-docdate (when encoded-docdate (format-time-string "%F" encoded-docdate))))
    (if (and denote-date-prompt-use-org-read-date
             (require 'org nil :no-error))
        (org-read-date nil
                       nil
                       nil
                       (concat "DOCDATE " (or prompt-text "for the new file"))
                       nil
                       initial-docdate)
      (read-string
       (format-prompt (concat "DOCDATE (e.g. 2022-06-16) "
                              (or prompt-text "for the new file"))
                      nil)
       initial-docdate
       'denote-date-history))))

(defun denote-paperless-doctype-prompt (&optional initial-doctype prompt-text)
  "Prompt for a document type string.

With optional INITIAL-DOCTYPE use it as the initial minibuffer text.
With optional PROMPT-TEXT use it in the minibuffer instead of the default prompt.

Always returns a string." 
  (when (and initial-doctype (string-empty-p initial-doctype))
    (setq initial-doctype nil))
  (completing-read
   (format-prompt (concat "DOCTYPE "
                          (or prompt-text "for the new file"))
                  nil)
   ;; Allow empty string in completion
   (cons "" denote-paperless-known-doctypes)
   nil
   t
   initial-doctype))

(defun denote-paperless-correspondent-prompt (&optional initial-correspondent prompt-text)
  "Prompt for a correspondent string.

With optional INITIAL-CORRESPONDENT use it as the initial minibuffer text.
With optional PROMPT-TEXT use it in the minibuffer instead of the default prompt.

Always returns a string."
  (when (and initial-correspondent (string-empty-p initial-correspondent))
    (setq initial-correspondent nil))
  (completing-read
   (format-prompt (concat "CORRESPONDENT "
                          (or prompt-text "for the new file"))
                  nil)
   ;; Allow empty string in completion
   (cons "" denote-paperless-known-correspondents)
   nil
   t
   initial-correspondent))

(defconst denote-paperless-docdate-format "%Y%m%d"
  "Format of DOCDATE as a string.")

(defun denote-paperless--concat-subcomponents (&optional docdate correspondent doctype asn)
  "Create an identifier based on DOCDATE, CORRESPONDENT, DOCTYPE and ASN.

DOCDATE can be an encoded date (list or number) or a string.
If an empty string is passed for CORRESPONDENT, DOCTYPE or ASN, it acts as if
the argument was nil."
    (when (and correspondent (string-empty-p correspondent))
      (setq correspondent nil))
    (when (and doctype (string-empty-p doctype))
      (setq doctype nil))
    (when (and asn (string-empty-p asn))
      (setq asn nil))
    (let ((docdate (denote-paperless-valid-date-p docdate)))
      ;; Note: sub-components can be arranged however you like. The only exception
      ;; to this is docdate that MUST remain at the begining.
      (concat (when docdate (format-time-string denote-paperless-docdate-format docdate))
              (when correspondent (concat "%%" correspondent))
              (when doctype (concat "&&" doctype))
              (when asn (concat "##" asn)))))

(defun denote-paperless-sluggify-signature (str)
  "Make STR an appropriate slug for signature.

This is almost the same function as `denote-sluggify-signature' except that '#',
'%' and '&' are not removed."
  (downcase
   (denote-slug-put-equals
    (replace-regexp-in-string "[][{}!@$^*()+'\"?,.\|;:~`‘’“”/-]*" "" str))))

(defun denote-paperless--find-max-unused-id-as-number (identifiers)
  "Find the first unused identifier amongst the IDENTIFIERS hash table.

All identifiers are contained in the keys of IDENTIFIERS.
The first unused identifier is the maximum integer of IDENTIFIERS plus 1."
  (let ((ids (delq nil
                   (hash-table-keys identifiers))))
    (if ids
        (1+ (apply #'max ids))
      0)))

(defcustom denote-paperless-integer-identifier-format "%d"
  "Format of the identifier component as an integer.")

(defun denote-paperless-format-identifier (identifier)
  "Format IDENTIFIER to comply with my own requirement.

IDENTIFIER can be a string or an integer.  If it is a string, it is first
converted to a integer and then formatted according to
`denote-paperless-integer-identifier-format'."
  (let ((id (cond ((stringp identifier)
                   (denote-paperless-string-to-integer identifier))
                  ((integerp identifier)
                   identifier))))
    (when id (format denote-paperless-integer-identifier-format id))))

(denote-paperless-format-identifier "65")
(denote-paperless-format-identifier "")
(denote-paperless-format-identifier 65)
(denote-paperless-format-identifier nil)

(defun denote-paperless-generate-identifier-as-number (initial-id-str _date)
  "Generate a unique identifier based on existing denote identifiers.

If INITIAL-ID-STR is not already used, return it.  Else, create a unique
identifier based on existing denote identifiers (`denote-used-identifiers' or
`denote--get-all-used-ids').

All identifiers are compared as integers, so any extra string formatting (like
zero-padding) is abstracted away from the comparison to find a unique
identifier.

This is a reference function for ‘denote-get-identifier-function’.

Inspiration from `denote-generate-identifier-as-date'."
  (let ((denote-used-ids-str (or denote-used-identifiers (denote--get-all-used-ids)))
        (denote-used-ids-int (make-hash-table :test #'equal))
        (initial-id-int (denote-paperless-string-to-integer initial-id-str)))
    ;; Rebuild a new hash table with keys converted from string to integer
    (maphash (lambda (key value)
               (let ((key-int (denote-paperless-string-to-integer key)))
                 (unless (null key-int)
                   (puthash key-int value denote-used-ids-int))))
             denote-used-ids-str)
    ;; Convert every possible outcome to a standardized format
    (denote-paperless-format-identifier 
     (cond (;; Keep initial-id as-is if it does not already exist
            (and initial-id-int
                 (not (gethash initial-id-int denote-used-ids-int)))
            initial-id-int)
           (;; Otherwise create a unique identifier
            denote-used-ids-int
            (denote-paperless--find-max-unused-id-as-number denote-used-ids-int))
           (t
            0)))))

(defun denote-paperless--rename-get-file-info-from-prompts-or-existing (file)
  "Retrieve existing info from FILE and prompt according to `denote-prompts'.

This my own implementation of `denote--rename-get-file-info-from-prompts-or-existing'
to work within my custom denote-paperless workflow.

It is meant to temporarily replace `denote--rename-get-file-info-from-prompts-or-existing'
(eg: with a `let') when calling `denote-rename-file' in the context of a
denote-paperless workflow."
  (let* ((file-in-prompt (propertize (file-relative-name file) 'face 'denote-faces-prompt-current-name))
         (file-type (denote-filetype-heuristics file))
         (date (denote-retrieve-front-matter-date-value file file-type))
         (identifier (or (denote-retrieve-filename-identifier file) ""))
         (signature (or (denote-retrieve-filename-signature file) ""))
         (title (or (denote-retrieve-title-or-filename file file-type) ""))
         (keywords (denote-extract-keywords-from-path file))
         ;; denote-paperless subcomponents
         (correspondent (or (denote-paperless-retrieve-correspondent signature) ""))
         (doctype (or (denote-paperless-retrieve-doctype signature) ""))
         (docdate (or (denote-paperless-retrieve-docdate signature) ""))
         (asn (or (denote-paperless-retrieve-asn signature) "")))
    (dolist (prompt denote-prompts)
      (pcase prompt
        ('identifier
         (setq identifier (denote-identifier-prompt
                           identifier
                           (format "Rename `%s' with IDENTIFIER (empty to remove)" file-in-prompt))))
        ('title
         (setq title (denote-title-prompt
                      title
                      (format "Rename `%s' with TITLE (empty to remove)" file-in-prompt))))
        ('keywords
         (setq keywords (denote-keywords-prompt
                         (format "Rename `%s' with KEYWORDS (empty to remove)" file-in-prompt)
                         (string-join keywords ","))))
        ('asn
         (setq asn (denote-paperless-asn-prompt
                    asn
                    (format "to rename `%s' with (empty to remove)" file-in-prompt))))
        ('docdate
         (setq docdate (denote-paperless-valid-date-p
                        (denote-paperless-docdate-prompt
                         docdate
                         (format "to rename `%s' with (empty to revove)" file-in-prompt)))))
        ('doctype
         (setq doctype (denote-paperless-doctype-prompt
                        doctype
                        (format "to rename `%s' with (empty to remove)" file-in-prompt))))
        ('correspondent
         (setq correspondent (denote-paperless-correspondent-prompt
                              correspondent
                              (format "to rename `%s' with (empty to remove)" file-in-prompt))))
        ))
    (setq signature (denote-paperless--concat-subcomponents docdate correspondent doctype asn))
    (list title keywords signature date identifier)))

(defun denote-paperless-rename-file ()
  "Rename file according to the denote-paperless file-naming scheme.

This is a simple wrapper to `denote-rename-file' that uses
`denote-paperless-generate-identifier-as-number' to uniquify identifiers and
`denote-paperless--rename-get-file-info-from-prompts-or-existing' to prompt the
user if required by `denote-prompts'.

This command is mainly meant to be used to rename documents, i.e. file without
front-matter.

`denote-prompts' accepts slightly different symbols compared to its default
denote behavior.  Within the context of
`denote-paperless--rename-get-file-info-from-prompts-or-existing' it now
accepts:

- `title': Same as default Denote.

- `keywords': Same as default Denote.

- `identifier': Same as default Denote.

- `asn': Prompts for an ASN (Archive Serial Number), an integer.  The ASN acts
  as a single source of truth for all the documents one have to keep physically,
  while still using the denote-paperless workflow.  If one needs to keep a
  document in its physical form: (1) write the ASN on the document before
  scanning, (2) scan the document, (3) store the document in a binder where all
  documents are sorted by ascending ASN, and (4) assign the ASN to the digital
  file with this command.

  The idea behind this process is to never have to use the physical binders to
  find a document.  If you need a specific physical document, you may find it by
  using the ASN in the file name to find the physical document in the binder.

  The workflow is more detailed here:
  https://docs.paperless-ngx.com/usage/#usage-recommended-workflow

  Warning: validity or duplicity of the ASN is not checked to leave more
  flexibility to the user.  However, duplicity can be checked with
  `denote-paperless-dired-show-duplicate-asns'.

- `docdate': Prompts for a document date.  This can be whatever date
  (date the document was created, digitalized or even added to the
  denote-paperless workflow), but it is recommended to use the document creation
  date.  This date is most likely already written somewhere on the document.
  E.g. on a letter, it is the date the letter was written.

- `doctype': Prompts for a document type among a list of prepopulated types.
  A document type is what the document is about, such as a letter, an invoice or
  a certificate.  To add a new type use the `denote-paperless-add-doctype'
  command, or edit the file `denote-paperless-known-doctypes-file' and call
  `denote-paperless-refresh-doctypes' right after.

- `correspondent': Prompts for a document correspondent among a list of
  prepopulated correspondents.  A correspondent is the person, institution or
  company that a document either originates from, or is sent to.  To add a new
  correspondent use the `denote-paperless-add-correspondent' command, or edit
  the file `denote-paperless-known-correspondents-file' and call
  `denote-paperless-refresh-correspondents' right after.

For each component the minibuffer is prepopulated with its existing value, if
any.  The user can then modify it accordingly.  An empty input means to remove
the component altogether.  All non-prompted components remain intact.

Within denote-paperless `asn', `docdate', `doctype', and `correspondent' are all
subcomponents of the `signature' component (note that it cannot be used anymore
in `denote-prompts').  Denote's file-naming scheme remains untouched, components
can still be re-ordered with `denote-file-name-components-order'.  The only
difference is that `signature' is now used to include all these new
subcomponents in the file name.  Its file-naming scheme is the following:

    DOCDATE%%CORRESPONDENT&&DOCTYPE##ASN

IMPORTANT: to make this new naming-scheme work properly, you MUST use a
different signature sluggification function than the default one provided by
denote in order to keep the new separators ('%', '&' and '#').  Fortunately,
this package provides a function that works out-of-the-box:
`denote-paperless-sluggify-signature'.  It is your duty to customize the
variable `denote-file-name-slug-functions' with this new function.

Instead of timestamps, identifiers are now simple incrementing integers.  This
allows documents to be referenced easily without ever touching the `identifier'
again.

Note: A combo of `docdate', `doctype' and `correspondent' was initially a good
candidate for the `identifier' but was quickly discarded as multiple documents
could have the same key.

Inspiration from: https://protesilaos.com/codelog/2025-09-20-emacs-denote-custom-identifiers/"
  (declare (interactive-only t))
  (interactive)
  ;; Temporarily replace the denote function by my own function with cl-letf
  (cl-letf (((symbol-function 'denote--rename-get-file-info-from-prompts-or-existing)
             (symbol-function 'denote-paperless--rename-get-file-info-from-prompts-or-existing)))
    (let ((denote-get-identifier-function #'denote-paperless-generate-identifier-as-number))
      (call-interactively 'denote-rename-file))))

(defun denote-paperless-rename-file-identifier ()
  "Convenience command to change the identifier of a file.
Like ‘denote-rename-file’, but prompts only for the identifier.

Inspiration from `denote-rename-file-identifier'"
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(identifier)))
    (call-interactively #'denote-paperless-rename-file)))

(defun denote-paperless-rename-file-title ()
  "Convenience command to change the title of a file.
Like ‘denote-rename-file’, but prompts only for the title.

Inspiration from `denote-rename-file-identifier'"
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(title)))
    (call-interactively #'denote-paperless-rename-file)))

(defun denote-paperless-rename-file-keywords ()
  "Convenience command to change the keywords of a file.
Like ‘denote-rename-file’, but prompts only for the keywords.

Inspiration from `denote-rename-file-identifier'"
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(keywords)))
    (call-interactively #'denote-paperless-rename-file)))

(defun denote-paperless-rename-file-signature ()
  "Convenience command to change the signature of a file.
Like ‘denote-rename-file’, but prompts only for the correspondent, the document
type, the document date and the ASN (in this order).

Inspiration from `denote-rename-file-identifier'"
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(correspondent doctype docdate asn)))
    (call-interactively #'denote-paperless-rename-file)))

(defun denote-paperless-rename-file-asn ()
  "Convenience command to change the Archive Serial Number (ASN) of a file.
Like ‘denote-rename-file’, but prompts only for the ASN.

Inspiration from `denote-rename-file-signature'"
  (declare (interactive-only t))
  (interactive)  
  (let ((denote-prompts '(asn)))
    (call-interactively #'denote-paperless-rename-file)))
  
(defun denote-paperless-rename-file-docdate ()
  "Convenience command to change the document date of a file.
Like ‘denote-rename-file’, but prompts only for the document date.

Inspiration from `denote-rename-file-identifier'"
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(docdate)))
    (call-interactively #'denote-paperless-rename-file)))
  
(defun denote-paperless-rename-file-doctype ()
  "Convenience command to change the document type of a file.
Like ‘denote-rename-file’, but prompts only for the document type.

Inspiration from `denote-rename-file-identifier'"
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(doctype)))
    (call-interactively #'denote-paperless-rename-file)))

(defun denote-paperless-rename-file-correspondent ()
  "Convenience command to change the correspondent of a file.
Like ‘denote-rename-file’, but prompts only for the correspondent.

Inspiration from `denote-rename-file-identifier'"
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(correspondent)))
    (call-interactively #'denote-paperless-rename-file)))

(defun denote-paperless-dired-rename-files-individually ()
  "Rename marked files in Dired, prompting the user for each file.

This is a simple wrapper to `denote-dired-rename-files', almost identical to
`denote-paperless-rename-file'.

See `denote-paperless-rename-file' docstring for more information."
  (declare (interactive-only t))
  (interactive)
  ;; Temporarily replace the denote function by my own function with cl-letf
  (cl-letf (((symbol-function 'denote--rename-get-file-info-from-prompts-or-existing)
             (symbol-function 'denote-paperless--rename-get-file-info-from-prompts-or-existing)))
    (let ((denote-get-identifier-function #'denote-paperless-generate-identifier-as-number))
      (call-interactively 'denote-dired-rename-files))))

(defalias
  'denote-paperless-dired-rename-marked-files-individually
  'denote-paperless-dired-rename-files-individually
  "Alias for `denote-paperless-dired-rename-files'.")

(defun denote-paperless-dired-rename-marked-files-individually-title ()
  "Convenience command to change the title of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-individually’, but prompts only for
the title."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(title)))
    (call-interactively #'denote-paperless-dired-rename-files-individually)))

(defun denote-paperless-dired-rename-marked-files-individually-keywords ()
  "Convenience command to change the keywords of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-individually’, but prompts only for
the keywords."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(keywords)))
    (call-interactively #'denote-paperless-dired-rename-files-individually)))

(defun denote-paperless-dired-rename-marked-files-individually-signature ()
  "Convenience command to change the signature of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-individually’, but prompts only for
the correspondent, the document type, the document date and the ASN (in this
order)."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(correspondent doctype docdate asn)))
    (call-interactively #'denote-paperless-dired-rename-files-individually)))

(defun denote-paperless-dired-rename-marked-files-individually-asn ()
  "Convenience command to change the ASN of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-individually’, but prompts only for
the ASN."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(asn)))
    (call-interactively #'denote-paperless-dired-rename-files-individually)))

(defun denote-paperless-dired-rename-marked-files-individually-docdate ()
  "Convenience command to change the document date of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-individually’, but prompts only for
the document date."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(docdate)))
    (call-interactively #'denote-paperless-dired-rename-files-individually)))

(defun denote-paperless-dired-rename-marked-files-individually-doctype ()
  "Convenience command to change the document type of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-individually’, but prompts only for
the document type."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(doctype)))
    (call-interactively #'denote-paperless-dired-rename-files-individually)))

(defun denote-paperless-dired-rename-marked-files-individually-correspondent ()
  "Convenience command to change the correspondent of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-individually’, but prompts only for
the correspondent."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-prompts '(correspondent)))
    (call-interactively #'denote-paperless-dired-rename-files-individually)))

(defcustom denote-paperless-prompts-multiple '(title keywords)
  "Specify the prompts followed by relevant Denote-paperless commands.

Serve the same purpose than `denote-prompts' but only for the commands:
`denote-paperless-dired-rename-files-collectively'.

The value of this user option is a list of symbols, which includes any
of the following: `title', `keywords', `asn', `docdate', `doctype', and `correspondent'.

See `denote-paperless-rename-file' docstring for more information.")

(defun denote-paperless-dired-rename-files-collectively ()
  "Rename marked files in Dired, prompting the user once for all files.

Customize `denote-paperless-prompts-multiple' the same way as `denote-prompts'
to define what the user will be prompted for. The available symbols are:
`title', `keywords', `asn', `docdate', `doctype', and `correspondent'.

See `denote-paperless-rename-file' docstring for more information.

Does not prompt the user for the final file name confirmation.

Inspiration from `denote-dired-rename-marked-files--change-keywords'."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (if-let* ((marks (dired-get-marked-files)))
      (let ((denote-prompts '()) ; Because we set this to an empty list, we don't need to use the cl-letf trick
            (denote-rename-confirmations nil)
            (denote-get-identifier-function #'denote-paperless-generate-identifier-as-number)
            new-title
            new-keywords
            new-signature
            new-asn
            new-docdate
            new-doctype
            new-correspondent)
        (dolist (prompt denote-paperless-prompts-multiple)
          ;; Prompt the user for the appropriate component(s)
          (pcase prompt
            ;; identifier is not relevant to prompt for
            ('title
             (setq new-title (denote-title-prompt
                              nil
                              "Rename marked files with TITLE (empty to remove)")))
            ('keywords
             (setq new-keywords (denote-keywords-prompt
                                 "Rename marked files with KEYWORDS, overwriting existing (empty to ignore/remove)")))
            ('asn
             (setq new-asn (denote-paperless-asn-prompt
                            nil
                            "to rename marked files with (empty to remove)")))
            ('docdate
             (setq new-docdate (denote-paperless-valid-date-p
                                (denote-paperless-docdate-prompt
                                 nil
                                 "to rename marked files with (empty to remove)"))))
            ('doctype
             (setq new-doctype (denote-paperless-doctype-prompt
                                nil
                                "to rename marked files with (empty to remove)")))
            ('correspondent
             (setq new-correspondent (denote-paperless-correspondent-prompt
                                      nil
                                      "to rename marked files with (empty to remove)")))
            ))
        (dolist (file marks)
          ;; Loop on each file
          (pcase-let* ((`(,title ,keywords ,signature ,date ,identifier)
                        (denote--rename-get-file-info-from-prompts-or-existing file))
                       (asn (or (denote-paperless-retrieve-asn signature) ""))
                       (correspondent (or (denote-paperless-retrieve-correspondent signature) ""))
                       (doctype (or (denote-paperless-retrieve-doctype signature) ""))
                       (docdate (or (denote-paperless-retrieve-docdate signature) "")))
            (dolist (prompt denote-paperless-prompts-multiple)
              ;; Assign a new value to the appriopriate component(s)
              (pcase prompt
                ('title
                 (setq title new-title))
                ('keywords
                 (setq keywords (denote-keywords-sort (denote-keywords--combine :replace new-keywords keywords))))
                ('asn
                 (setq asn new-asn))
                ('docdate
                 (setq docdate new-docdate))
                ('doctype
                 (setq doctype new-doctype))
                ('correspondent
                 (setq correspondent new-correspondent))))
            ;; Build the complete signature and rename file
            (setq signature (denote-paperless--concat-subcomponents docdate correspondent doctype asn))
            (denote--rename-file file title keywords signature date identifier)))
    (denote-update-dired-buffers))
  (user-error "No files to rename; aborting")))

(defalias
  'denote-paperless-dired-rename-marked-files-collectively
  'denote-paperless-dired-rename-files-collectively
  "Alias for `denote-paperless-dired-rename-files'.")

(defun denote-paperless-dired-rename-marked-files-collectively-title ()
  "Convenience command to change the title of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-collectively’, but prompts only for
the title."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote-paperless-prompts-multiple '(title)))
    (denote-paperless-dired-rename-files-collectively)))

(defun denote-paperless-dired-rename-marked-files-collectively-keywords ()
  "Convenience command to change the keywords of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-collectively’, but prompts only for
the keywords."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote-paperless-prompts-multiple '(keywords)))
    (denote-paperless-dired-rename-files-collectively)))

(defun denote-paperless-dired-rename-marked-files-collectively-signature ()
  "Convenience command to change the signature of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-collectively’, but prompts only for
for the correspondent, the document type, the document date and the ASN (in this
order)."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote-paperless-prompts-multiple '(docdate correspondent doctype asn)))
    (denote-paperless-dired-rename-files-collectively)))

(defun denote-paperless-dired-rename-marked-files-collectively-asn ()
  "Convenience command to change the ASN of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-collectively’, but prompts only for
the ASN."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote-paperless-prompts-multiple '(asn)))
    (denote-paperless-dired-rename-files-collectively)))

(defun denote-paperless-dired-rename-marked-files-collectively-docdate ()
  "Convenience command to change the document date of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-collectively’, but prompts only for
the document date."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote-paperless-prompts-multiple '(docdate)))
    (denote-paperless-dired-rename-files-collectively)))

(defun denote-paperless-dired-rename-marked-files-collectively-doctype ()
  "Convenience command to change the document type of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-collectively’, but prompts only for
the document type."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote-paperless-prompts-multiple '(doctype)))
    (denote-paperless-dired-rename-files-collectively)))

(defun denote-paperless-dired-rename-marked-files-collectively-correspondent ()
  "Convenience command to change the correspondent of marked files in Dired.
Like ‘denote-paperless-dired-rename-files-collectively’, but prompts only for
the correspondent."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (let ((denote-paperless-prompts-multiple '(correspondent)))
    (denote-paperless-dired-rename-files-collectively)))

(defun denote-paperless-faces-signature-matcher (limit)
  "Match the signature in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-signature-matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "==\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "==\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-paperless-faces-identifier-matcher (limit)
  "Match a general identifier in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-identifier--matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "@@\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "@@\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-paperless-faces-title-matcher (limit)
  "Match the title in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-title-matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "--\\(?1:[^/]*?\\)\\(@@\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "--\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-paperless-faces-keywords-matcher (limit)
  "Match the keywords in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-keywords-matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "__\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "__\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-paperless-faces-asn-matcher (limit)
  "Match the ASN in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-signature-matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "##\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "##\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-paperless-faces-docdate-matcher (limit)
  "Match the docdate in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-signature-matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "==\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "==\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-paperless-faces-doctype-matcher (limit)
  "Match the doctype in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-signature-matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "&&\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "&&\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defun denote-paperless-faces-correspondent-matcher (limit)
  "Match the correspondent in a Dired line, not looking beyond LIMIT.

Copy-pasta of `denote-faces-signature-matcher'."
  (let ((initial-match-data (match-data))
        (initial-point (point)))
    (if (or (re-search-forward "%%\\(?1:[^/]*?\\)\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\|\\.\\)[^/]*$" limit t)
            (re-search-forward "%%\\(?1:[^/]*\\)$" limit t))
        (progn
          (goto-char (match-end 1))
          (set-match-data (list (match-beginning 1) (match-end 1)))
          (point))
      (goto-char initial-point)
      (set-match-data initial-match-data)
      nil)))

(defgroup denote-paperless-faces ()
  "Faces for Denote-paperless."
  :group 'denote-paperless)
 
(defface denote-paperless-faces-identifier '((t :inherit font-lock-variable-name-face))
  "Face for file name identifier in Dired buffers."
  :group 'denote-paperless-faces
  :package-version '(denote-paperless . "0.1.0"))
 
(defface denote-paperless-faces-asn '((t :inherit font-lock-warning-face))
  "Face for file name ASN in Dired buffers."
  :group 'denote-paperless-faces
  :package-version '(denote-paperless . "0.1.0"))

(defface denote-paperless-faces-docdate '((t :inherit font-lock-warning-face))
  "Face for file name docdate in Dired buffers."
  :group 'denote-paperless-faces
  :package-version '(denote-paperless . "0.1.0"))

(defface denote-paperless-faces-doctype '((t :inherit font-lock-warning-face))
  "Face for file name doctype in Dired buffers."
  :group 'denote-paperless-faces
  :package-version '(denote-paperless . "0.1.0"))

(defface denote-paperless-faces-correspondent '((t :inherit font-lock-warning-face))
  "Face for file name correspondent in Dired buffers."
  :group 'denote-paperless-faces
  :package-version '(denote-paperless . "0.1.0"))

(defconst denote-paperless-faces-matchers
  `((denote-faces-directory-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-subdirectory nil t))
    ;; Docdate with format 00000000
    ("\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9]\\{2\\}\\)\\(?3:[0-9]\\{2\\}\\)"
     (goto-char (match-beginning 0)) ; pre-form, executed before looking for the first identifier
     (goto-char (match-end 0))       ; post-form, executed after all matches (identifiers here) are found
     (1 'denote-faces-year nil t)
     (2 'denote-faces-month nil t)
     (3 'denote-faces-day nil t))
    ;; Identifier with general format (not yet possible)
    (denote-paperless-faces-identifier-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-paperless-faces-identifier nil t))
    ;; Title
    (denote-paperless-faces-title-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-title nil t))
    ;; Keywords
    (denote-paperless-faces-keywords-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-keywords nil t))
    ;; ;; Signature
    ;; (denote-paperless-faces-signature-matcher
    ;;  (goto-char (match-beginning 0))
    ;;  (goto-char (match-end 0))
    ;;  (0 'denote-faces-signature nil t))
    ;; ASN
    (denote-paperless-faces-asn-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-paperless-faces-asn nil t))
    ;; Docdate
    (denote-paperless-faces-docdate-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-paperless-faces-docdate nil t))
    ;; Doctype
    (denote-paperless-faces-doctype-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-paperless-faces-doctype nil t))
    ;; Correspondent
    (denote-paperless-faces-correspondent-matcher
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-paperless-faces-correspondent nil t))
    ;; Delimiters
    ("\\(@@\\|--\\|__\\|==\\|%%\\|&&\\|##\\)"
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-delimiter nil t))
    ;; Extension
    ("\\..*$"
     (goto-char (match-beginning 0))
     (goto-char (match-end 0))
     (0 'denote-faces-extension nil t)))
  "Matchers for fontification of file names.

Modified version of `denote-faces-matchers'.")

(defconst denote-paperless-faces-file-name-keywords-for-dired
  `((denote-faces-dired-file-name-matcher ,@denote-paperless-faces-matchers))
  "Keywords for fontification of file names.

Copy-pasta of `denote-faces-file-name-keywords-for-dired'.")

(defun denote-paperless-dired-add-font-lock (&rest _)
  "Append `denote-paperless-faces-file-name-keywords' to font lock keywords.

Copy-pasta of `denote-dired-add-font-lock'"
  (when (derived-mode-p 'dired-mode)
    (font-lock-add-keywords nil denote-paperless-faces-file-name-keywords-for-dired t)))

(defun denote-paperless-dired-remove-font-lock (&rest _)
  "Remove `denote-paperless-faces-file-name-keywords' from font lock keywords.

Copy-pasta of `denote-dired-remove-font-lock'"
  (when (derived-mode-p 'dired-mode)
    (font-lock-remove-keywords nil denote-paperless-faces-file-name-keywords-for-dired)))

;;;###autoload
(define-minor-mode denote-paperless-dired-mode
  "Fontify all Denote-paperless-style file names.
Add this or `denote-dired-mode-in-directories' to `dired-mode-hook'.

Copy-pasta of `denote-dired-mode'."
  :global nil
  :group 'denote-paperless-dired
  (if denote-paperless-dired-mode
      (progn
        (denote-paperless-dired-add-font-lock)
        (advice-add #'wdired-change-to-wdired-mode :after #'denote-paperless-dired-add-font-lock)
        (advice-add #'wdired-finish-edit :after #'denote-paperless-dired-add-font-lock))
    (denote-paperless-dired-remove-font-lock)
    (advice-remove #'wdired-change-to-wdired-mode #'denote-paperless-dired-add-font-lock)
    (advice-remove #'wdired-finish-edit #'denote-paperless-dired-add-font-lock))
  (font-lock-flush (point-min) (point-max)))

(defun denote-paperless--get-files-in-dir (directory)
  "Return files returned by `denote-directory-files' as if DIRECTORY was
`denote-directory'."
  (let ((denote-directory directory))
    (denote-directory-files)))

(defun denote-paperless-find-duplicate-asns (directory)
  "Find all files in DIRECTORY with a duplicate ASN.

Do not exclude the first occurrence of a duplicate like `delete-dups' would do."
  (let* ((files (denote-paperless--get-files-in-dir directory))
         (asns (delq nil
                     (mapcar #'denote-paperless-retrieve-asn
                             (mapcar #'denote-retrieve-filename-signature files)))))
    (seq-filter
     ;; This is tricky but this predicate works: Check that a asn appears
     ;; twice in the asns list. Idea from https://emacs.stackexchange.com/a/53015
     (lambda (file)
       (let ((asn (denote-paperless-retrieve-asn (denote-retrieve-filename-signature file))))
         (member asn (cdr (member asn asns)))))
     files)))

(defun denote-paperless-dired-show-duplicate-asns (directory)
  "Put duplicate ASNs from DIRECTORY in a dedicated Dired buffer."
  (interactive
   (list
    (read-directory-name "Select DIRECTORY to check for duplicate ASNs: " default-directory)))
  (if-let ((duplicates (denote-paperless-find-duplicate-asns directory)))
      (dired (cons (format "Denote duplicate ASNs" directory) duplicates))
    (message "No duplicate ASN in `%s'" directory)))

(defun denote-paperless-find-duplicate-identifiers (directory)
  "Find all files in DIRECTORY with a duplicate IDENTIFIER.

Do not exclude the first occurrence of a duplicate like `delete-dups' would do."
  (let* ((files (denote-paperless--get-files-in-dir directory))
         (identifiers (delq nil (mapcar #'denote-retrieve-filename-identifier files))))
    (seq-filter
     ;; This is tricky but this predicate works: Check that a identifier appears
     ;; twice in the identifiers list. Idea from https://emacs.stackexchange.com/a/53015
     (lambda (file)
       (let ((identifier (denote-retrieve-filename-identifier file)))
         (member identifier (cdr (member identifier identifiers)))))
     files)))

(defun denote-paperless-dired-show-duplicate-identifiers (directory)
  "Put duplicate identifiers from DIRECTORY in a dedicated Dired buffer."
  (interactive
   (list
    (read-directory-name "Select DIRECTORY to check for duplicate identifiers: " default-directory)))
  (if-let* ((duplicates (denote-paperless-find-duplicate-identifiers directory)))
      (dired (cons (format "Denote duplicate identifiers" directory) duplicates))
    (message "No duplicates identifiers in `%s'" directory)))

(defcustom denote-paperless-basket-directory "~/Downloads/"
  "Directory path of the paperless basket.

The paperless basket is meant to be a place to copy denote-paperless files to,
so that they can be sent to other people. This is generally useful when one
needs to prepare an application that requires multiple documents to be sent to a
third-party.

Must end with a forward slash '/'.")

(defun denote-paperless-dired-copy-to-basket ()
  "Copy marked files in Dired to the basket directory.

The basket directory can be customized with `denote-paperless-basket-directory'.

The destination file name is cleaned up (only the document date and the title
are kept), ready to be sent to a third-party.  An auto-incrementing counter is
added to the destination file name if a file with the same name already exists
at this location.

The original file remains untouched."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (if-let* ((marks (dired-get-marked-files)))
      (progn
        (dolist (file marks)
          (let* ((suffix (file-name-extension file t))
                 (title (denote-retrieve-filename-title file))
                 (signature (denote-retrieve-filename-signature file))
                 (docdate (denote-paperless-retrieve-docdate signature))
                 (newpath-no-suffix (concat denote-paperless-basket-directory
                                            (format-time-string denote-paperless-docdate-format
                                                                docdate)
                                            "-"
                                            title)))
            (let ((counter 0)
                  (newpath (concat newpath-no-suffix suffix)))
              (while (file-exists-p newpath)
                (setq newpath (concat newpath-no-suffix
                                      "_"
                                      (number-to-string counter)
                                      suffix))
                (setq counter (1+ counter)))
              (copy-file file newpath)
              (message "Copied file to: %s" newpath))))
        (denote-update-dired-buffers))
    (user-error "No files to rename; aborting")))

(provide 'denote-paperless)
;;; denote-paperless.el ends here
