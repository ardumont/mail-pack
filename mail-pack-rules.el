;;; mail-pack-rules.el --- Ease refiling rules for mail-pack -*- lexical-binding: t; -*-

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
(require 'f)

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

(defun mail-pack-rules--find-maildir (msg)
  "Given a MSG, determine the maildir.
Implementation detail: Use msg's :path entry."
  (let ((path (mu4e-message-field msg :path)))
    (->> mail-pack-accounts
         (-map (-compose (-partial #'concat mu4e-maildir) (-partial #'concat "/") #'car)) ;; find all maildirs
         (--filter (f-ancestor-of? it path))
         car)))

(defun mail-pack-rules--maybe-create-maildir (dir)
  "Create create maildir DIR if it does not exist yet.
Return t if the dir already existed, or an attempt has been made to
create it -- we cannot be sure creation succeeded here, since this
is done asynchronously.
Otherwise, return nil.
Note, DIR has to be an absolute path."
  (when (and (file-exists-p dir) (not (file-directory-p dir)))
    (mu4e-error "%s exists, but is not a directory" dir))
  (if(file-directory-p dir)
      t
    (mu4e~proc-mkdir dir)
    t))

(defun mail-pack-rules--destination-folder-with-account (msg dest)
  "Given a MSG, determines the full destination folder with the right account.
DEST is the agnostic destination folder described in a rule."
  (let ((full-maildir (mail-pack-rules--find-maildir msg)))
    (list :full-path (concat full-maildir dest)
          :relative (concat "/" (f-filename full-maildir) dest))))

(defun mail-pack-rules--compute-destination (msg dest-rule)
  "Compute the destination rule from the MSG.
DEST-RULE is the suffix destination.
This will create transparently the right destination folder if inexistant.
And return the relative destination rule folder for the right account."
  (let ((dest-data (mail-pack-rules--destination-folder-with-account msg dest-rule)))
    (mail-pack-rules--maybe-create-maildir (plist-get dest-data :full-path))
    (plist-get dest-data :relative)))

(defun mail-pack-rules-filter-expand-rule--from (rule)
  "Expand from RULE."
  (let ((from-rule (plist-get rule :from))
        (dest-rule (plist-get rule :dest)))
    (lambda (msg)
      (when (mu4e-message-contact-field-matches msg :from from-rule)
        (mail-pack-rules--compute-destination msg dest-rule)))))

(defun mail-pack-rules-filter-expand-rule--to (rule)
  "Expand to RULE."
  (let ((to-rule (plist-get rule :to))
        (dest-rule (plist-get rule :dest)))
    (lambda (msg) (when (mu4e-message-contact-field-matches msg :to to-rule)
               (mail-pack-rules--compute-destination msg dest-rule)))))

(defun mail-pack-rules-filter-expand-rule--subject (rule)
  "Expand to subject RULE."
  (let ((subject-rule (plist-get rule :subject))
        (dest-rule (plist-get rule :dest)))
    (lambda (msg) (let ((subject (or (mu4e-message-field msg :subject) "")))
               (when (and subject-rule
                          (string-match subject-rule subject))
                 (mail-pack-rules--compute-destination msg dest-rule))))))

(defun mail-pack-rules-filter-expand-rule--from-subject (rule)
  "Expand to `'from`' and `'subject`' RULE."
  (let ((from-rule    (plist-get rule :from))
        (subject-rule (plist-get rule :subject))
        (dest-rule    (plist-get rule :dest)))
    (lambda (msg) (let ((subject (or (mu4e-message-field msg :subject) "")))
               (when (and subject-rule
                          (mu4e-message-contact-field-matches msg :from from-rule)
                          (string-match subject-rule subject))
                 (mail-pack-rules--compute-destination msg dest-rule))))))

(defun mail-pack-rules-filter-expand-rule--to-subject (rule)
  "Expand to `'to`' and `'subject`' RULE."
  (let ((from-rule    (plist-get rule :to))
        (subject-rule (plist-get rule :subject))
        (dest-rule    (plist-get rule :dest)))
    (lambda (msg) (let ((subject (or (mu4e-message-field msg :subject) "")))
               (when (and subject-rule
                          (mu4e-message-contact-field-matches msg :to from-rule)
                          (string-match-p subject-rule subject))
                 (mail-pack-rules--compute-destination msg dest-rule))))))

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

(defun mail-pack-rules-filter-msg-by-rules (rules msg &optional default-folder)
  "Given a list of RULES, filter the MSG.
Optionally, DEFAULT-FOLDER can be set."
  (let ((archive-default-folder (if default-folder default-folder mail-pack-rules-default-archive-folder)))
    (if rules
        (-if-let (folder (->> (mail-pack-rules-fn rules archive-default-folder)
                              (-map (lambda (rule-fn) (apply rule-fn (list msg))))
                              (-drop-while #'null)
                              car))
            folder
          archive-default-folder)
      archive-default-folder)))

(defun mail-pack-rules-filter-msg (msg &optional default-folder)
  "Filter MSG according to `'mail-pack-rules-refiling-rules`'.
DEFAULT-FOLDER is the fallback folder."
  (mail-pack-rules-filter-msg-by-rules mail-pack-rules-refiling-rules msg default-folder))

(custom-set-variables '(mail-pack-rules-refiling-rules
                        '((:from "@travis-ci.org"                   :dest "/ci/travis")
                          (:from "@*apple.com"                      :dest "/ads" :subject "apple\\|apple developer")
                          (:from "@*sfeir.com"                      :dest "/sfeir")
                          (:from "@*linkedin.com"                   :dest "/job/linkedin")
                          (:from "@*viadeo.com"                     :dest "/job/viadeo")
                          (:from "@*ovh.com"                        :dest "/hosting/ovh")
                          (:from "@*monster.com"                    :dest "/job/monster")
                          (:from "@octo.com"                        :dest "/job/ads")
                          (:from "@*youtube.com"                    :dest "/newsletters/youtube")
                          (:from "@*youtube.com"                    :dest "/newsletters/youtube")
                          (:from "@*ucpa.com"                       :dest "/newsletters/ucpa")
                          (:from "@*ucpa.com"                       :dest "/newsletters/ucpa")
                          (:from "stumpwm-devel-request@nongnu.org" :dest "/newsletters/stumpwm")
                          (:from "@*sourceforge.net"                :dest "/newsletters/sourceforge")
                          (:from "@*sourceforge.net"                :dest "/newsletters/sourceforge")
                          (:from "@*jules.com"                      :dest "/ads")
                          (:from "@*musikia*"                       :dest "/ads")
                          (:from "@*grosbill.com"                   :dest "/ads")
                          (:from "@*grosbill.com"                   :dest "/ads")
                          (:from "@*paypal.fr"                      :dest "/newsletter/paypal")
                          (:from "@*.infoq.com"                     :dest "/newsletters/infoq")
                          (:from "@*twitter.com"                    :dest "/social/twitter")
                          (:from "@*meetup.com"                     :dest "/social/meetup")
                          (:from "@*ovh.com"                        :dest "/hosting/ovh")
                          (:from "@*plus.google.com"                :dest "/social/google-plus")
                          (:from "@*heroku.com"                     :dest "/dev/heroku")
                          (:from "@*myheritage.com"                 :dest "/newsletter/myheritage")
                          (:from "@*facebookmail.com"               :dest "/social/facebook")
                          (:from "@*eventbrite.com"                 :dest "/social/meetup")
                          (:from "@dropbox.com"                     :dest "/share/dropbox")
                          (:from "@*doodle.com"                     :dest "/social/meetup")
                          (:from "@*journaldumanagement.com"        :dest "/newsletters/jdn")
                          (:from "@*github.com"                     :dest "/dev/github")
                          (:from "@*free-mobile.com"                :dest "/newsletters/free")
                          (:from "clojure-fr@googlegroups.com"      :dest "/newsletters/clojure-fr")
                          (:from "@*ugc.fr"                         :dest "/newsletters/ugc")
                          (:to "swh-devel@inria.fr"                 :dest "/swh/devel")
                          (:to "password-store@lists.zx2c4.com"     :dest "/newsletters/password-store")
                          (:to "nix-dev@lists.science.uu.nl"        :dest "/newsletters/nix-dev")
                          (:to "@*meetup.com"                       :dest "/social/meetup")
                          (:to "xmonad@haskell.org"                 :dest "/newsletters/xmonad")
                          (:to "emacs-orgmode@gnu.org"              :dest "/newsletters/org")
                          (:to "emacs-live@googlegroups.com"        :dest "/newsletters/emacs-live")
                          (:to "help-gnu-emacs@gnu.org"             :dest "/newsletters/help-emacs")
                          (:to "paris-devops@googlegroups.com"      :dest "/newsletters/paris-devops"))))

(provide 'mail-pack-rules)
;;; mail-pack-rules.el ends here
