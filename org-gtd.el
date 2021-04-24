;;; org-gtd2.el --- An implementation of GTD for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pierre Glandon
;;
;; Author: Pierre Glandon <https://github.com/pglandon>
;; Maintainer: Pierre Glandon <pglandon78@gmail.com>
;; Created: March 26, 2021
;; Modified: March 26, 2021
;; Version: 0.0.1
;; Keywords: GTD
;; Homepage: https://github.com/pglandon/org-gtd2
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  An implementation of GTD for Emacs
;;
;;; Code:

(defconst org-gtd-actionable-file-basename "actionable"
"Name of Org file listing all actionable items.")

(defconst org-gtd-inbox-file-basename "inbox"
"Name of Org file listing all captured items.")

(defconst org-gtd-incubate-file-basename "incubate"
"Name of Org file listing all someday/maybe items.")

(defconst org-gtd-project-file-basename "project"
"We follow templates convention. Variable value has no effect.")

(defconst org-gtd-projects-base-dir "Projects/"
"Relative path of directory listing all GTD Projects.")

(defconst org-gtd-actionable-template
"#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CANCELED(c@)\n\n* Actions
:PROPERTIES:
:CATEGORY: Action
:END:\n\n* Delegated
:PROPERTIES:
:CATEGORY: Delegated
:END:\n\n* Scheduled
:PROPERTIES:
:CATEGORY: Scheduled
:END:
"
"Template for the GTD actionable list.")

(defconst org-gtd-project-template
"#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CANCELED(c@)
#+FILETAGS: :PROJECT:\n* ?F
:PROPERTIES:
:TRIGGER:  next-sibling todo!(NEXT)
:END:
"
"Template for a GTD project. Replace ?F with filename.")

(defconst org-gtd-inbox-template
"#+STARTUP: overview hidestars logrefile indent logdone
#+TODO: NEXT TODO WAIT | DONE CANCELED TRASH
#+begin_comment
This is the inbox. Everything goes in here when you capture it.
#+end_comment
"
"Template for the GTD inbox.")

(defconst org-gtd-incubate-template
"#+begin_comment
Here go the things you want to think about someday. Review this file as often
as you feel the need: every two months? Every six months? Every year?
It's suggested that you categorize the items in here somehow, such as:
\"to read\", \"to buy\", \"to eat\", etc - whatever works best for your mind!
#+end_comment
"
"Template for the GTD someday/maybe list.")

(defconst org-gtd-actions   ".*Actions")
(defconst org-gtd-delegated ".*Delegated")
(defconst org-gtd-incubate  ".*Incubate.*")
(defconst org-gtd-scheduled ".*Scheduled")

(defgroup org-gtd nil
  "Customize the org-gtd package."
  :version 0.1
  :group 'emacs)

(defcustom org-gtd-directory "~/gtd/"
  "Directory of Org based GTD files.
This is the directory where to look for the files used in
this Org-mode based GTD implementation."
  :type 'directory)

(defvar org-gtd-command-map (make-sparse-keymap)
  "Keymap for function `org-gtd-user-input-mode', a minor mode.")

(defun org-gtd--init ()
  "Initialize org-gtd."
  (org-gtd--find-or-create-and-save-files))

(defun org-gtd--find-or-create-and-save-files ()
  (mapcar
   (lambda (buffer) (with-current-buffer buffer (save-buffer) buffer))
   `(,(org-gtd--actions-file) ,(org-gtd--inbox-file))))

(defun org-gtd--path (file)
  "Return the full path to FILE.org.
This assumes the file is located in `org-gtd-directory'."
  (f-join org-gtd-directory (concat file ".org")))

(defun org-gtd--gtd-file (gtd-type &optional gtd-file)
  "Return a buffer to GTD-TYPE.org.
Create the file and template first if it doesn't already exist.
GTD-FILE is a special argument to override filename and build template for projects."
  (let* ((file-path (org-gtd--path (or gtd-file gtd-type)))
         (file-buffer (find-file-noselect file-path)))
    (or (f-file-p file-path)
        (with-current-buffer file-buffer
          (org-mode)
          (insert (replace-regexp-in-string "\?F" (file-name-nondirectory gtd-file) (symbol-value
                   (intern
                    (string-join
                     `("org-gtd-" ,gtd-type "-template"))))))
          (org-mode-restart)
          (save-buffer)))
    file-buffer))

(defun org-gtd--inbox-file ()
"Create or return the buffer to the GTD inbox file."
(org-gtd--gtd-file org-gtd-inbox-file-basename))

(defun org-gtd--actions-file ()
"Create or return the buffer to the GTD inbox file."
(org-gtd--gtd-file org-gtd-actionable-file-basename))

(defun org-gtd--projects-dir-path ()
"Return projects' directory path."
(f-join org-gtd-directory org-gtd-projects-base-dir)
)

(defun org-gtd--projects-files-path ()
"Return projects glob path."
(f-join (org-gtd--projects-dir-path) "*.org")
)

(defun org-gtd--project-path (project-title)
"Return project relative path"
(concat org-gtd-projects-base-dir project-title))

(defun org-gtd--project-file (project-title)
"Create or return the buffer to the GTD project file."
(org-gtd--gtd-file org-gtd-project-file-basename (org-gtd--project-path project-title)))

(defun org-gtd-capture (&optional GOTO KEYS)
  "Capture something into the GTD inbox.

Wraps the function `org-capture' to ensure the inbox exists.

For GOTO and KEYS, see `org-capture' documentation for the variables of the same name."
  (interactive)
  (kill-buffer (org-gtd--inbox-file))
  (let ((org-capture-templates org-gtd-capture-templates))
    (org-capture GOTO KEYS)
    )
  )

(defun org-gtd-process-inbox ()
  "Process the GTD inbox.
Use this once a day and/or weekly as part of the weekly review."
  (interactive)
  (set-buffer (org-gtd--inbox-file))
  (display-buffer (org-gtd--inbox-file))

  ;; (org-gtd--find-or-create-and-save-files)
  (org-map-entries
   (lambda ()
     (setq org-map-continue-from (org-element-property
                                  :begin
                                  (org-element-at-point)))
     (org-narrow-to-element)
     (org-show-subtree)
     (org-gtd--process-inbox-element)
     (widen)))
  (setq-local header-line-format nil)
  ;; (org-gtd--find-or-create-and-save-files))
(with-current-buffer (org-gtd--inbox-file)
  (save-buffer)
  (kill-buffer)
  )
  )

(defun org-gtd--process-inbox-element ()
  "With point on an item, choose which GTD action to take."
  (let ((action
         (read-multiple-choice
          "What to do with this item?"
          '((?q "quick" "quick item: < 2 minutes, done!")
            (?p "project" "transform current inbox into a project.")
            (?t "throw out" "this has no value to me.")
            (?s "single action" "do this when possible.")
            (?T "tag element" "add a tag to the element.")
            (?S "schedule element" "add a schedule to the element.")
            (?D "deadline" "add a deadline to the element.")
            (?E "effort" "set the effort of the element.")
            (?P "priority" "set the priority of the element.")
            (?C "clarify" "set the tags, priority and efforts.")
            (?F "free edit" "enter special mode to edit element before processing it.")
            (?r "refile" "refile inbox.")))))
    (cl-case (car action)
      (?q (org-gtd--quick-action))
      (?p (org-gtd--project))
      (?t (org-gtd--trash))
      (?s (org-gtd--single-action))
      (?T (org-gtd--tag-element))
      (?S (org-gtd--schedule))
      (?D (org-gtd--deadline))
      (?E (org-gtd--effort))
      (?P (org-gtd--priority))
      (?F (org-gtd--free-edit))
      (?C (org-gtd--clarify))
      (?r (org-gtd--refile)))))

(defun org-gtd--quick-action ()
  "Process GTD inbox item by doing it now.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Mark it as done and archive."
  ;; (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--trash ()
  "Mark GTD inbox item as cancelled and archive it."
  ;; (org-gtd--clarify-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "CANCELED")
  (org-archive-subtree))

(defun org-gtd--project ()
"Process GTD inbox item by transforming it into a project."
  (goto-char (point-min))
  (let* ((project-title (org-element-property :title (org-element-at-point)))
    (project-target (org-gtd--refile-project-target project-title)))
    (org-refile nil nil project-target)
    (merge-headings (org-gtd--project-file project-title))))

(defun org-gtd--single-action ()
  "Process GTD inbox item as a single action.
Allow the user apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox.  Set as a NEXT action and refile to
`org-gtd-actionable-file-basename'."
  (goto-char (point-min))
  (org-todo "NEXT")
  (org-refile nil nil (org-gtd--refile-actions-target)))

(defun org-gtd--tag-element ()
  "Allow the user to apply user-defined tags from
`org-tag-persistent-alist', `org-tag-alist' or file-local tags in
the inbox."
(goto-char (point-min))
(org-set-tags-command))

(defun org-gtd--schedule ()
"Allow the user to apply org-schedule in the inbox."
(goto-char (point-min))
(org-schedule 0)
)

(defun org-gtd--deadline ()
"Allow the user to apply org-deadline in the inbox."
        (goto-char (point-min))
        (org-deadline 0))

(defun org-gtd--effort ()
"Allow the user to apply org-effort in the inbox."
        (goto-char (point-min))
        (org-set-effort))

(defun org-gtd--priority ()
"Allow the user to apply org-effort in the inbox."
        (goto-char (point-min))
        (org-priority))

(defun org-gtd--clarify ()
"Allow the user to apply org-tags, priority and effort in the inbox."
        (goto-char (point-min))
        (org-set-tags-command)
        (org-priority)
        (org-set-effort))

(define-minor-mode org-gtd-user-input-mode
  "Minor mode for org-gtd."
  nil "GTD " org-gtd-command-map
  (setq-local header-line-format
              (substitute-command-keys
               "\\<org-gtd-command-map>Clarify buffer.  Finish \
`\\[org-gtd-free-edit-finalize]'.")))

(defun org-gtd-free-edit-finalize ()
  "Finalize the clarify process."
  (interactive)
  (org-gtd-user-input-mode -1)
  (setq-local header-line-format "")
  (exit-recursive-edit))

(defun org-gtd--free-edit ()
"Allow the user to edit the inbox."
        (goto-char (point-min))
        (org-gtd-user-input-mode 1)
        (recursive-edit))

(defun org-gtd--refile ()
  "Refile inbox."
  (org-refile nil nil t))

(defun org-gtd--refile-target (heading-regexp targets)
  "Filters refile targets from TARGETS using HEADING-REGEXP."
  (let* ((org-refile-targets targets)
         (results (cl-find-if
                   (lambda (rfloc)
                     (string-match heading-regexp
                                   (car rfloc)))
                   (org-refile-get-targets))))
    results))

(defun org-gtd--refile-actions-targets ()
  `((,(org-gtd--path org-gtd-actionable-file-basename) :maxlevel . 1)))

(defun org-gtd--refile-actions-target ()
  (org-gtd--refile-target org-gtd-actions (org-gtd--refile-actions-targets)))

(defun org-gtd--refile-project-targets ()
"Return targets for projects."
  `((,((org-gtd--projects-files-path)) :maxlevel . 10)))

(defun org-gtd--refile-project-target (project-title)
  "Create project file named PROJECT-TITLE and return a refile target pointing to it."
(org-gtd--project-file project-title)
  (let ((project-path (org-gtd--path (org-gtd--project-path project-title))))
(list nil project-path nil nil)
        ))

(defun org-gtd--refile-project-target-or-create-it (project-title)
(let ((project-target (org-gtd--refile-project-target project-title)))
(unless project-target
(org-gtd--project-file project-title)
(setq project-target (org-gtd--refile-project-target project-title)))
project-target))

(defconst org-gtd-capture-templates `(("i" "Inbox"
entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
"* %?\n%U\n\n  %i"
:kill-buffer t)
("p" "Project entry"
entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
"* %(completing-read \"Project: \"
(org-gtd--get-projects-list)
nil nil)\n** %? \n%i")
("l" "TODO with link"
entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
"* %?\n%U\n\n  %i\n  %a"
:kill-buffer t))
"Templates for Org GTD Capture. Must be in the same format as Org Capture.")

(defun get-headings-position (org-buffer level)
  "Return an a-list with key the name of the heading at LEVEL in the ORG-BUFFER and value a list of their point (position) in the file."
  (let ((level (or level 1))
        (result))
    (with-current-buffer org-buffer
      (org-map-entries
       (lambda () (let ((heading (org-element-property :title (org-element-at-point))))
                    (if (assoc heading result)
                        (push (point) (cdr (assoc heading result)))
                      (map-put result heading (list (point))))))
       (concat "+LEVEL=" (number-to-string level))))
    result))

(defun merge--headings (level)
  "Merge headings with the same name in current buffer at LEVEL.
Side effects include entries sorting by alphabetical order."
                                        ; We sort headings at cursor position.
  (ignore-errors (org-sort-entries t ?a))

                                        ; We merge subtrees if same headings.
  (let* ((level level) (headings (get-headings-position (current-buffer) level)))
    (dolist (heading headings)
      (let ((positions (sort (cdr heading) '<)))
        (when (> (length positions) 1)
          (let ((to-merge (cdr positions)))
            (dolist (to-merge-element to-merge)
              (goto-char to-merge-element)
              (kill-whole-line)))))))

                                        ; We do this again for each subtree.
  (org-map-entries
   (lambda () (progn
                (org-narrow-to-element)
                (merge--headings (+ 1 level))
                (widen)))
   (concat "+LEVEL=" (number-to-string level))))

(defun merge-headings (org-buffer)
  "Merge headings in ORG-BUFFER."
  (with-current-buffer org-buffer
    (goto-char (point-min))
    (merge--headings 1)))



(org-gtd--init)

(provide 'org-gtd)

;;; org-gtd.el ends here.
