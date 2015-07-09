;;; mail-pack-rules.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  ardumont

;; Author: ardumont <eniotna.t@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Define mail-pack rules for refiling mails

;;

;;; Code:

(require 'dash)
(require 'dash-functional)

(defcustom mail-pack-rules-default-archive-folder "/archive"
  "Default archive folder if no rules.")

;; FIXME call a function hook to transform list of rules into function rule
(defcustom mail-pack-rules-refiling-rules nil
  "Define rules to easily refile emails.
Possible example:
  (custom-set-variables '(mail-pack-rules-refiling-rules
                          '((:from \"@travis-ci.org\"   :dest \"/ci/travis\")
                            (:from \"@apple.com\"       :dest \"/ads\" :subject \"apple\\\\|apple developer\")
                            (:from \"@sfeir.com\"       :dest \"/sfeir\")
                            (:from \"@linkedin.com\"    :dest \"/job/linkedin\")
                            (:from \"@viadeo.com\"      :dest \"/job/viadeo\")
                            (:from \"@ovh.com\"         :dest \"/hosting/ovh\")
                            (:from \"@*monster.com\"    :dest \"/job/monster\")
                            (:from \"@octo.com\"        :dest \"/job/ads\")
                            (:to \"swh-devel@inria.fr\" :dest \"/swh/devel\"))))"
  :type 'list
  :require 'mail-pack-rules
  :group 'mail-pack)

(defun mail-pack-rules-filter-expand-rule--from (rule)
  "Expand from RULE."
  (let ((from-rule (plist-get rule :from))
        (dest-rule (plist-get rule :dest)))
    (lambda (msg)
      (when (mu4e-message-contact-field-matches msg :from from-rule)
        dest-rule))))

(defun mail-pack-rules-filter-expand-rule--to (rule)
  "Expand to RULE."
  (let ((to-rule (plist-get rule :to))
        (dest-rule (plist-get rule :dest)))
    (lambda (msg) (when (mu4e-message-contact-field-matches msg :to to-rule)
               dest-rule))))

(defun mail-pack-rules-filter-expand-rule--subject (rule)
  "Expand to subject RULE."
  (let ((subject-rule (plist-get rule :subject))
        (dest-rule (plist-get rule :dest)))
    (lambda (msg) (let ((subject (or (mu4e-message-field msg :subject) "")))
               (when (and subject-rule
                          (string-match subject-rule subject))
                 dest-rule)))))

(defun mail-pack-rules-filter-expand-rule--from-subject (rule)
  "Expand to `'from`' and `'subject`' RULE."
  (let ((from-rule    (plist-get rule :from))
        (subject-rule (plist-get rule :subject))
        (dest-rule    (plist-get rule :dest)))
    (lambda (msg) (let ((subject (or (mu4e-message-field msg :subject) "")))
               (when (and subject-rule
                          (mu4e-message-contact-field-matches msg :from from-rule)
                          (string-match subject-rule subject))
                 dest-rule)))))

(defun mail-pack-rules-filter-expand-rule--to-subject (rule)
  "Expand to `'to`' and `'subject`' RULE."
  (let ((from-rule    (plist-get rule :to))
        (subject-rule (plist-get rule :subject))
        (dest-rule    (plist-get rule :dest)))
    (lambda (msg) (let ((subject (or (mu4e-message-field msg :subject) "")))
               (when (and subject-rule
                          (mu4e-message-contact-field-matches msg :to from-rule)
                          (string-match-p subject-rule subject))
                 dest-rule)))))

(defun mail-pack-rules-filter-expand-rule--default (default-folder)
  "Expand DEFAULT-FOLDER rule."
  (let ((default default-folder))
    (lambda (_) default)))

(defun mail-pack-rules-filter-expand-rule (rule default-folder)
  "Expand rule to a refiling function.
If RULE is not defined or no rule is found, the DEFAULT-FOLDER is used."
  (cond ((not (plist-member rule :dest)) ;; no dest folder, default
         (mail-pack-rules-filter-expand-rule--default default-folder))
        ((and (plist-member rule :from)
              (plist-member rule :subject))
         (mail-pack-rules-filter-expand-rule--from-subject rule))
        ((and (plist-member rule :to)
              (plist-member rule :subject))
         (mail-pack-rules-filter-expand-rule--to-subject rule))
        ((plist-member rule :to)
         (mail-pack-rules-filter-expand-rule--to rule))
        ((plist-member rule :from)
         (mail-pack-rules-filter-expand-rule--from rule))
        ((plist-member rule :subject)
         (mail-pack-rules-filter-expand-rule--subject rule))
        (t
         (mail-pack-rules-filter-expand-rule--default default-folder))))

(defun mail-pack-rules-fn (rules default-folder)
  "Compute RULES to rule function.
DEFAULT-FOLDER used as a default rule function."
  (-map (-rpartial #'mail-pack-rules-filter-expand-rule default-folder) rules))

(defun mail-pack-rules-filter-msg (rules msg &optional default-folder)
  "Given a list of RULES, filter the MSG.
Optionally, DEFAULT-FOLDER can be set."
  (let ((archive-default-folder (if default-folder default-folder "/archive")))
    (if mail-pack-rules-refiling-rules
        (->> (mail-pack-rules-fn mail-pack-rules-refiling-rules archive-default-folder)
             (-map (lambda (rule-fn) (apply rule-fn (list msg))))
             (-drop-while #'null)
             car)
      archive-default-folder)))

(custom-set-variables '(mail-pack-rules-refiling-rules
                        '((:from "@travis-ci.org"   :dest "/ci/travis")
                          (:from "@apple.com"       :dest "/ads" :subject "apple\\|apple developer")
                          (:from "@sfeir.com"       :dest "/sfeir")
                          (:from "@linkedin.com"    :dest "/job/linkedin")
                          (:from "@viadeo.com"      :dest "/job/viadeo")
                          (:from "@ovh.com"         :dest "/hosting/ovh")
                          (:from "@*monster.com"    :dest "/job/monster")
                          (:from "@octo.com"        :dest "/job/ads")
                          (:to "swh-devel@inria.fr" :dest "/swh/devel"))))

(provide 'mail-pack-rules)
;;; mail-pack-rules.el ends here
