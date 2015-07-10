(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(require 'mail-pack)

(expectations
 (expect 2 (mail-pack/--nb-accounts
            '(("machine" "email-description")
              ("machine" "imap.gmail.com")
              ("machine" "smtp.gmail.com")
              ("machine" "2-email-description")
              ("machine" "2-imap.gmail.com")
              ("machine" "2-smtp.gmail.com")
              ("machine" "blog")
              ("machine" "blog-description")
              ("machine" "irc")))))


(expectations
 ;; case ok
 (expect '(("machine" "email-description") ("machine" "imap.gmail.com") ("machine" "smtp.gmail.com") ("machine" "other-machine"))
         (with-temp-file "/tmp/.authinfo"
           (insert "machine email-description\nmachine imap.gmail.com\nmachine smtp.gmail.com\nmachine other-machine")
           (mail-pack/setup-possible-p "/tmp/.authinfo")))

 ;; invalid cases

 ;; file does not exist
 (expect nil
         (mail-pack/setup-possible-p "/tmp/.authinfo-that-does-not-exist"))

 ;; missing email-description
 (expect nil
         (with-temp-file "/tmp/.authinfo"
           (insert "machine imap.gmail.com\nmachine smtp.gmail.com\nmachine other-machine")
           (mail-pack/setup-possible-p "/tmp/.authinfo")))

 ;; missing imap.gmail.com
 (expect nil
         (with-temp-file "/tmp/.authinfo"
           (insert "machine email-description\nmachine smtp.gmail.com\nmachine other-machine")
           (mail-pack/setup-possible-p "/tmp/.authinfo")))

 ;; missing smtp.gmail.com
 (expect nil
         (with-temp-file "/tmp/.authinfo"
           (insert "machine email-description\nmachine imap.gmail.com\nmachine other-machine")
           (mail-pack/setup-possible-p "/tmp/.authinfo"))))

(expectations
 (expect "some-label" (mail-pack/--label "" "some-label"))
 (expect "some-label" (mail-pack/--label nil "some-label"))
 (expect "1-some-label" (mail-pack/--label "1" "some-label")))

(expectations
 (expect "a b c" (mail-pack/--compute-fullname "a" "b" "c"))
 (expect "a b" (mail-pack/--compute-fullname "a" "b" nil))
 (expect "a b" (mail-pack/--compute-fullname "a" "b" ""))
 (expect "a" (mail-pack/--compute-fullname "a" nil nil))
 (expect "a" (mail-pack/--compute-fullname "a" "" ""))
 (expect "" (mail-pack/--compute-fullname nil nil nil))
 (expect "" (mail-pack/--compute-fullname "" "" ""))
 (expect "b c" (mail-pack/--compute-fullname "" "b" "c")))

(expectations
 (expect "some-address" (mail-pack/--maildir-from-email "some-address@some-server.com")))

(expectations
 (expect '("some-account0" "some-account1") (mail-pack/--maildir-accounts '(("some-account0" ("some" "thing"))
                                                                            ("some-account1" ("thing" "some"))))))

(expectations
 (expect '("some-account@some-server.com" "some-account@elsewhere.com")
         (mail-pack/--find-account '("some-other-account@something" "some-account@some-server.com" "some-account@elsewhere.com") "some-account"))
 (expect nil
         (mail-pack/--find-account '("some-other-account@something" "some-account@some-server.com" "some-account@elsewhere.com") "unknown")))

(expectations
 (expect "some-account"
         (mail-pack/--retrieve-account '(:docid "176039"
                                                :subject "some subject"
                                                :date (21347 3862 0)
                                                :size "some-size"
                                                :message-id "some-message-id"
                                                :path "some-path"
                                                :maildir /INBOX
                                                :priority normal
                                                :flags (seen)
                                                :parts ((:index 1 :name 1.part :mime-type text/plain :type (leaf) :attachment nil :size 5454)
                                                        (:index 2 :name 2.part :mime-type text/html :type (leaf) :attachment nil :size 89456))
                                                :from ((Clojure Users . groups-noreply@linkedin.com))
                                                :to (("username" . "some-account@somewhere.com"))
                                                :cc (("some other username" . "some-other-account@elsewhere.com"))
                                                :bcc (("yet another username" . "yet-another-account@elsewhere.com"))
                                                :body-txt "some-body-text-plain-separated-then-other-html-content")
                                       '("some-account" "some-not-found-account"))))
