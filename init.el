(require 'gnus)

;; adding the creds.el lib
(live-add-pack-lib "creds.el")

;; got this line from one of the tutorials. Seemed interesting enough
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")

;; standard way of getting imap going
(setq gnus-select-method
         '(nnimap "gmail"
          (nnimap-address "imap.gmail.com")
          (nnimap-server-port 993)
          (nnimap-stream ssl)))

;; set up smtp so we can send from gmail too:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials "~/.authinfo"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;http://www.emacswiki.org/cgi-bin/wiki/GnusGmail
;;http://linil.wordpress.com/2008/01/18/gnus-gmail/

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads are nice!
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

;; load the entry imap.gmail.com in the ~/.netrc, we obtain a hash-map with the needed data
(setq description (get-creds (read-lines "~/.authinfo") "description"))

(setq full-name (concat (get-entry description "firstname") " "
                        (get-entry description "surname") " "
                        (get-entry description "name")))

(setq x-url (get-entry description "x-url"))
(setq mail-address (get-entry description "mail"))
(setq mail-host (get-entry description "mail-host"))

(setq gnus-posting-styles
       '((".*"
          (name full-name)
          ("X-URL" x-url)
          (mail-host-address mail-host))))

(setq send-mail-function 'smtpmail-send-it)
