(require 'ert)
(require 'ert-expectations)
(require 'el-mock)

(load-file "init.el")

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
