;;; mail-pack-rules-tests.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine Romain Dumont

;; Author: Antoine Romain Dumont <antoine.romain.dumont@gmail.com>
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

;;

;;; Code:

(require 'ert)

(ert-deftest test-mail-pack-rules-filter-expand-rule--from ()
  (should (string= "/somewhere"
                   (let ((test-msg '(:subject "some subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule--from '(:from "foobar@some-news.com" :dest "/somewhere")) (list test-msg)))))
  (should-not (let ((test-msg '(:subject "some super uber top mega cool subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--from '(:from "barfoo@some-news.com" :dest "/elsewhere")) (list test-msg))))
  (should-not (let ((test-msg '(:subject "some other dumbass subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--from nil) (list test-msg)))))

(ert-deftest test-mail-pack-rules-filter-expand-rule--subject ()
  (should (string= "/somewhere"
                   (let ((test-msg '(:subject "some [hello] subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule--subject '(:subject "\\[hello\\]"
                                                                                    :dest "/somewhere")) (list test-msg)))))
  (should-not (let ((test-msg '(:subject "some super uber top mega cool subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--subject '(:subject "\\[hello\\]"
                                                                               :dest "/somewhere")) (list test-msg))))
  (should-not (let ((test-msg '(:subject "some other dumbass subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--subject nil) (list test-msg)))))

(ert-deftest test-mail-pack-rules-filter-expand-rule--to ()
  (should (string= "/somewhere"
                   (let ((test-msg '(:subject "some subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule--to '(:to "some@mail.com" :dest "/somewhere")) (list test-msg)))))
  (should-not (let ((test-msg '(:subject "some super uber top mega cool subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--to '(:to "someone@else.fr" :dest "/elsewhere")) (list test-msg))))
  (should-not (let ((test-msg '(:subject "some other dumbass subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--to nil) (list test-msg)))))

(ert-deftest test-mail-pack-rules-filter-expand-rule--from-subject ()
  (should (string= "/somewhere"
                   (let ((test-msg '(:subject "[BLA] some hype subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule--from-subject
                             '(:from "foobar@some-news.com"
                                     :subject "\\[BLA\\]"
                                     :dest "/somewhere")) (list test-msg)))))
  (should-not (let ((test-msg '(:subject "some super uber top mega cool subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--from-subject
                        '(:from "foobar@some-news.com"
                                :subject "\\[BLA\\]"
                                :dest "/somewhere")) (list test-msg))))
  (should-not (let ((test-msg '(:subject "[BLA] some super uber top mega cool subject"
                                         :from (("someone" . "someother@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--from-subject
                        '(:from "foobar@some-news.com"
                                :subject "\[BLA\]"
                                :dest "/somewhere")) (list test-msg))))
  (should-not (let ((test-msg '(:subject "some other dumbass subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--from-subject nil) (list test-msg)))))

(ert-deftest test-mail-pack-rules-filter-expand-rule--to-subject ()
  (should (string= "/somewhere"
                   (let ((test-msg '(:subject "[BLA] some hype subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule--to-subject
                             '(:to "some@mail.com"
                                   :subject "\\[BLA\\]"
                                   :dest "/somewhere")) (list test-msg)))))
  (should-not (let ((test-msg '(:subject "some super uber top mega cool subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--to-subject
                        '(:to "some@mail.com"
                              :subject "\\[BLA\\]"
                              :dest "/somewhere")) (list test-msg))))
  (should-not (let ((test-msg '(:subject "[BLA] some super uber top mega cool subject"
                                         :from (("someone" . "someother@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--to-subject
                        '(:to "foobar@some-news.com"
                              :subject "\[BLA\]"
                              :dest "/somewhere")) (list test-msg))))
  (should-not (let ((test-msg '(:subject "some other dumbass subject"
                                         :from (("Foobar" . "foobar@some-news.com"))
                                         :to (("Antoine Dumont" . "some@mail.com")))))
                (apply (mail-pack-rules-filter-expand-rule--to-subject nil) (list test-msg)))))


(ert-deftest test-mail-pack-rules-filter-expand-rule--to-subject ()
  (should (string= "/default-folder"
                   (let ((test-msg '(:subject "some other dumbass subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule--default "/default-folder") (list test-msg)))))
  (should-not (let ((test-msg "something-else-and-still-works"))
                (apply (mail-pack-rules-filter-expand-rule--to-subject nil) (list test-msg)))))

(ert-deftest test-mail-pack-rules-filter-expand-rule ()
  (should (string= "/default" ;; no :dest so bad rule so default folder
                   (let ((test-msg '(:subject "[BLA] some hype subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule '(:from "foobar@some-news.com") "/default") (list test-msg)))))

  (should (string= "/in/the-jungle" ;; `'from`' ok
                   (let ((test-msg '(:subject "[BLA] some hype subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule '(:from "foobar@some-news.com" :dest "/in/the-jungle") "/default") (list test-msg)))))

  (should (string= "/over/the-rainbow" ;; `'to`' ok
                   (let ((test-msg '(:subject "[BLA] some hype subject"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule '(:to "some@mail.com" :dest "/over/the-rainbow") "/default") (list test-msg)))))

  (should (string= "/in/the-sky" ;; `'subject-to`' ok
                   (let ((test-msg '(:subject "[LAB] exciting news!"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule '(:subject "exciting news" :to "some@mail.com" :dest "/in/the-sky") "/default")
                            (list test-msg)))))

  (should (string= "/under/the-sea" ;; `'subject-from`' ok
                   (let ((test-msg '(:subject "[LAB] exciting news!"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule '(:subject "exciting news" :from "@some-news.com" :dest "/under/the-sea") "/default")
                            (list test-msg)))))

  (should (string= "/default" ;; no rule so default
                   (let ((test-msg '(:subject "[LAB] match not!"
                                              :from (("Foobar" . "foobar@some-news.com"))
                                              :to (("Antoine Dumont" . "some@mail.com")))))
                     (apply (mail-pack-rules-filter-expand-rule nil "/default") (list test-msg))))))

(ert-deftest test-mail-pack-rules-filter-msg ()
  (should (equal
           '("/ads" "/ci/travis" "/sfeir" "/job/linkedin" "/job/viadeo" "/hosting/ovh" "/job/ads" "/job/monster" "/swh/devel" "/spam" "/archive")
           (let ((mail-pack-rules-refiling-rules '((:from "@apple.com"       :dest "/ads" :subject "apple\\|apple developer")
                                                   (:from "@travis-ci.org"   :dest "/ci/travis")
                                                   (:from "@sfeir.com"       :dest "/sfeir")
                                                   (:from "@linkedin.com"    :dest "/job/linkedin")
                                                   (:from "@viadeo.com"      :dest "/job/viadeo")
                                                   (:from "@ovh.com"         :dest "/hosting/ovh")
                                                   (:from "@*monster.com"    :dest "/job/monster")
                                                   (:from "@octo.com"        :dest "/job/ads")
                                                   (:to "swh-devel@inria.fr" :dest "/swh/devel")
                                                   (:subject "\\[blabla\\]"  :dest "/spam")
                                                   (:from "badrule-because-no-dest")))
                 (msgs '((:subject "Eat some shiny apple"
                                   :from (("apple" . "apple@apple.com"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "[travis] broken tests"
                                   :from (("travis" . "travis@travis-ci.org"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "sfeir newsletter"
                                   :from (("sfeir" . "so@sfeir.com"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "linkedin news on your network"
                                   :from (("linkedin" . "spam@linkedin.com"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "someone checked out your viadeo profile"
                                   :from (("viadeo" . "spam@viadeo.com"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "hosting resources stop!"
                                   :from (("ovh" . "ovh@ovh.com"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "some octo thingy"
                                   :from (("octo" . "octo@octo.com"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "some ssii wants to check your ass!"
                                   :from (("monster" . "monster@monster.com"))
                                   :to (("Antoine Dumont" . "me@me.fr")))
                         (:subject "checkout that awesome code!"
                                   :from (("me" . "me@me.fr"))
                                   :to (("SWH devel" . "swh-devel@inria.fr")))
                         (:subject "[blabla] some spam you will be delighted with!"
                                   :to (("me" . "me@me.fr")))
                         (:subject "no rule"
                                   :to (("me" . "me@me.fr"))))))
             (-map (-partial #'mail-pack-rules-filter-msg-by-rules
                             mail-pack-rules-refiling-rules)
                   msgs)))))

(ert-deftest test-mail-pack-rules--find-maildir ()
  (should (string-equal "/home/user/Maildir/a"
                        (let ((mail-pack-accounts '((:account-a (mu4e-maildir "/home/user/Maildir/a"))
                                                    (:account-b (mu4e-maildir "/home/user/Maildir/b")))))
                          (mail-pack-rules--find-maildir '(:path "/home/user/Maildir/a/INBOX/cur/1436484967_0.15992.corellia,U=31822,FMD5=7e33429f656f1e6e9d79b29c3f82c57e:2,RS")))))
  (should (string-equal "/home/user/Maildir/b"
                        (let ((mail-pack-accounts '((:account-a (mu4e-maildir "/home/user/Maildir/a"))
                                                    (:account-c (mu4e-maildir "/home/user/Maildir/c"))
                                                    (:account-b (mu4e-maildir "/home/user/Maildir/b")))))
                          (mail-pack-rules--find-maildir '(:path "/home/user/Maildir/b/INBOX/cur/abc_0.15992.corellia,U=31822,FMD5=7e33429f656f1e6e9d79b29c3f82c57e:2,RS"))))))

(ert-deftest test-mail-pack-rules--destination-folder-with-account ()
  (should (equal
           '(:full-path "/home/tony/.mails/eniotna.t/dev/null" :relative "/eniotna.t/dev/null")
           (mail-pack-rules--destination-folder-with-account '(:path "/home/tony/.mails/eniotna.t/inbox") "/dev/null"))))

(provide 'mail-pack-rules-tests)
;;; mail-pack-rules-tests.el ends here
