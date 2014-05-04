;;; mail-pack.el --- mail-pack

;;; Commentary:

;;; Code:

(install-packs '(s
                 dash
                 creds
                 google-contacts
                 offlineimap))

(require 'gnus)
(require 'creds)
(require 'dash)
(require 's)
(require 'smtpmail)
(require 'google-contacts)
(require 'google-contacts-gnus)
(require 'google-contacts-message)
(require 'offlineimap)

;; install mu in your system `sudo aptitude install -y mu`
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)

;; ===================== setup file

;; create your .authinfo file and and encrypt it in ~/.authinfo.gpg with M-x epa-encrypt-file
(defvar *MAIL-PACK-MAIL-ROOT-FOLDER* (expand-file-name "~/.mails"))
(defvar *MAIL-PACK-CREDENTIALS-FILE* (expand-file-name "~/.authinfo.gpg"))
(defvar *MAIL-PACK-PERIOD-FETCH-MAIL* 600 "Number of seconds between fetch + indexing. Default to 600 seconds.")

;; ===================== setup function

(defun mail-pack/log (str) "A log function for the pack."
  (message "Mail Pack - %s" str))

(defun mail-pack/setup-possible-p (creds-file)
  "Check if the setup is possible by checking the existence of the file creds-file
and that the entries 'imap.gmail.com', 'smtp.gmail.com', and 'email-description' are provided.
If all is ok, return the creds-file's content, nil otherwise."
  (when (file-exists-p creds-file)
    (let ((parsed-lines (creds/read-lines creds-file)))
      (when (and (creds/get parsed-lines "imap.gmail.com")
                 (creds/get parsed-lines "smtp.gmail.com")
                 (creds/get parsed-lines "email-description"))
        parsed-lines))))

(defun mail-pack/--nb-accounts (creds-file-content)
  "Compute how many 'email-description' entries exist? This corresponds to the number of accounts setuped."
  (--reduce-from (let ((machine (creds/get-entry it "machine")))
                   (if (string-match-p "email-description" machine)
                       (+ 1 acc)
                     acc))
                 0
                 creds-file-content))

(defun mail-pack/--find-account (emails-sent-to possible-account)
  "Given a list of recipients (to, cc, bcc) and a possible account, try to filter the filter with such account."
  (--filter (string= possible-account (mail-pack/--maildir-from-email it)) emails-sent-to))

(defun mail-pack/--compute-composed-message! ()
  "Compute the composed message (Delegate this to mu4e)."
  mu4e-compose-parent-message)

(defun mail-pack/--retrieve-account (composed-parent-message possible-accounts)
  "Try and retrieve the mail account to which the composed-parent-message was sent to (look into the :to, :cc, :bcc fields if we found the right account).
 If all accounts are found, return the first one."
  ;; build all the emails recipients (to, cc, bcc)
  (let ((emails-sent-to (mapcar #'cdr (concatenate #'list
                                                   (plist-get composed-parent-message :to)
                                                   (plist-get composed-parent-message :cc)
                                                   (plist-get composed-parent-message :bcc)))))
    ;; try to find the accounts the mail was sent to
    (-when-let (found-accounts (--mapcat (mail-pack/--find-account emails-sent-to it) possible-accounts))
      ;; return the account found
      (mail-pack/--maildir-from-email (car found-accounts)))))

(defun mail-pack/--maildir-accounts (accounts)
  "Given the list of accounts, return only the list of possible maildirs."
  (mapcar #'car accounts))

(defun mail-pack/set-account (accounts)
  "Set the account. When composing a new message, ask the user to choose. When replying/forwarding, determine automatically the account to use."
  (let* ((possible-accounts       (mail-pack/--maildir-accounts accounts))
         (composed-parent-message (mail-pack/--compute-composed-message!))
         ;; determine which account to use
         (account (if composed-parent-message
                      ;; when replying/forwarding a message
                      (mail-pack/--retrieve-account composed-parent-message possible-accounts)
                    ;; or otherwise, when composing a new email (we let the user choose which account (s)he wants to compose with)
                    (completing-read (format "Compose with account: (%s) " (mapconcat #'car accounts "/"))
                                     possible-accounts nil t nil nil (caar accounts))))
         ;; Then retrieving the main account vars setup
         (setup-account-vars (assoc account accounts)))
    (if setup-account-vars
        (mail-pack/--setup-as-main-account! setup-account-vars)
      (error "No email account found!"))))

(defun mail-pack/--setup-keybindings-and-hooks (my-mu4e-account-alist)
  "Install hooks and keybindings."
  (add-hook 'mu4e-headers-mode-hook
            (lambda ()
              (define-key 'mu4e-headers-mode-map (kbd "o") 'mu4e-headers-view-message)))

  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (define-key 'mu4e-main-mode-map (kbd "c") 'mu4e-compose-new)
              (define-key 'mu4e-main-mode-map (kbd "e") 'mu4e-compose-edit)
              (define-key 'mu4e-main-mode-map (kbd "f") 'mu4e-compose-forward)
              (define-key 'mu4e-main-mode-map (kbd "r") 'mu4e-compose-reply)))

  (global-set-key (kbd "C-c e m") 'mu4e)

  ;; Hook to determine which account to use before composing
  (add-hook 'mu4e-compose-pre-hook
            (lambda () (mail-pack/set-account my-mu4e-account-alist))))

(defun mail-pack/--label (entry-number label)
  "Given an entry number, compute the label"
  (if (or (null entry-number) (string= "" entry-number))
      label
    (format "%s-%s" entry-number label)))

(defun mail-pack/--common-configuration! ()
  "Install the common configuration between all accounts."
  (setq gnus-invalid-group-regexp "[:`'\"]\\|^$"
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash"
        ;; setup some handy shortcuts
        mu4e-maildir-shortcuts `(("/INBOX"             . ?i)
                                 (,mu4e-sent-folder    . ?s)
                                 (,mu4e-trash-folder   . ?t)
                                 (,mu4e-drafts-folder  . ?d))
        ;; don't save message to Sent Messages, GMail/IMAP will take care of this
        mu4e-sent-messages-behavior 'delete
        ;; allow for updating mail using 'U' in the main view
        mu4e-get-mail-command "offlineimap"
        ;; update every 5 min
        mu4e-update-interval *MAIL-PACK-PERIOD-FETCH-MAIL*
        mu4e-attachment-dir "~/Downloads"
        mu4e-view-show-images t
        ;; prefer plain text message
        mu4e-view-prefer-html nil
        ;; to convert html to plain text - prerequisite: aptitude install -y html2text
        mu4e-html2text-command "html2text -utf8 -width 120"
        ;; to convert html to plain text - prerequisite: aptitude install -y html2mardown
        ;; mu4e-html2text-command "html2markdown | grep -v '&nbsp_place_holder;'"
        ;; to convert html to org - prerequisite: aptitude install -y pandoc
        ;; mu4e-html2text-command "pandoc -f html -t org"
        ;; see mu4e-header-info for the full list of keywords
        mu4e-headers-fields '((:human-date    . 20)
                              (:flags         . 6)
                              (:from          . 30)
                              (:to            . 30)
                              (:mailing-list  . 10)
                              ;;(:tags          . 10)
                              (:subject))
        ;; see format-time-string for the format - here french readable
        mu4e-headers-date-format "%d/%m/%Y %H:%M"
        ;; universal date
        ;; mu4e-headers-date-format "%FT%T%z"
        ;; only consider email addresses that were seen in personal messages
        mu4e-compose-complete-only-personal t
        ;; auto complete addresses
        mu4e-compose-complete-addresses t
        message-kill-buffer-on-exit t
        ;; SMTP setup ; pre-requisite: gnutls-bin package installed
        message-send-mail-function    'smtpmail-send-it
        smtpmail-stream-type          'starttls
        starttls-use-gnutls           t
        smtpmail-smtp-service         587
        smtpmail-default-smtp-server  "smtp.gmail.com"
        smtpmail-smtp-server          "smtp.gmail.com"
        smtpmail-debug-info t
        smtpmail-debug-verb t
        ;; empty the hooks
        mu4e-headers-mode-hook nil
        mu4e-main-mode-hook nil
        mu4e-compose-pre-hook nil))

(defun mail-pack/--compute-fullname (firstname surname name)
  "Compute the user's fullname"
  (cl-flet ((if-null-then-empty (v) (if v v "")))
    (s-trim (format "%s %s %s" (if-null-then-empty firstname) (if-null-then-empty surname) (if-null-then-empty name)))))

(defun mail-pack/--maildir-from-email (mail-address)
  "Compute the maildir (without its root folder) from the mail-address."
  (car (s-split "@" mail-address)))

(defun mail-pack/--setup-as-main-account! (account-setup-vars)
  "Given the entry account vars, set the main account vars up."
  (mapc #'(lambda (var) (set (car var) (cadr var))) (cdr account-setup-vars)))

(defun mail-pack/--setup-account (creds-file creds-file-content &optional entry-number)
  "Setup one account. If entry-number is not specified, we are dealing with the main account. Other it's a secondary account."
  (let* ((description              (creds/get creds-file-content (mail-pack/--label entry-number "email-description")))
         (full-name                (mail-pack/--compute-fullname (creds/get-entry description "firstname")
                                                                 (creds/get-entry description "surname")
                                                                 (creds/get-entry description "name")))
         (x-url                    (creds/get-entry description "x-url"))
         (mail-address             (creds/get-entry description "mail"))
         (mail-host                (creds/get-entry description "mail-host"))
         (signature                (creds/get-entry description "signature-file"))
         (folder-mail-address      (mail-pack/--maildir-from-email mail-address))
         (folder-root-mail-address (format "%s/%s" *MAIL-PACK-MAIL-ROOT-FOLDER* folder-mail-address))
         ;; setup the account
         (account-setup-vars       `(,folder-mail-address
                                     ;; Global setup
                                     (user-mail-address      ,mail-address)
                                     (user-full-name         ,full-name)
                                     (message-signature-file ,signature)
                                     ;; GNUs setup
                                     (gnus-posting-styles ((".*"
                                                            (name ,full-name)
                                                            ("X-URL" ,x-url)
                                                            (mail-host-address ,mail-host))))
                                     ;; SMTP setup ; pre-requisite: gnutls-bin package installed
                                     (smtpmail-starttls-credentials ((,(mail-pack/--label entry-number "smtp.gmail.com") 587 ,mail-address nil)))
                                     (smtpmail-auth-credentials     ,creds-file)

                                     ;; mu4e setup
                                     (mu4e-maildir ,(expand-file-name folder-root-mail-address)))))
    ;; Sets the main account if it is the one!
    (unless entry-number
      (mail-pack/--setup-as-main-account! account-setup-vars))
    ;; In any case, return the account setup vars
    account-setup-vars))

(defvar my-mu4e-account-alist nil "Email Accounts list")

(defun mail-pack/setup (creds-file creds-file-content)
  "Mail pack setup"
  ;; common setup
  (mail-pack/--common-configuration!)

  ;; reinit the accounts list
  (setq my-mu4e-account-alist)

  ;; secondary accounts setup
  (-when-let (nb-accounts (mail-pack/--nb-accounts creds-file-content))
    (when (< 1 nb-accounts)
      (->> (number-sequence 2 nb-accounts)
        (--map (mail-pack/--setup-account *MAIL-PACK-CREDENTIALS-FILE* creds-file-content (format "%s" it)))
        (--map (add-to-list 'my-mu4e-account-alist it)))))

  ;; main account setup
  (add-to-list 'my-mu4e-account-alist (mail-pack/--setup-account creds-file creds-file-content))

  ;; install bindings and hooks
  (mail-pack/--setup-keybindings-and-hooks my-mu4e-account-alist))

;; ===================== setup routine

(-if-let (creds-file-content (mail-pack/setup-possible-p *MAIL-PACK-CREDENTIALS-FILE*))
    (progn
      (mail-pack/log (concat *MAIL-PACK-CREDENTIALS-FILE* " found! Running Setup..."))
      (mail-pack/setup *MAIL-PACK-CREDENTIALS-FILE* creds-file-content)
      (mail-pack/log "Setup done!"))
  (mail-pack/log (concat
                  "You need to setup your credentials file " *MAIL-PACK-CREDENTIALS-FILE* " for this to work. (The credentials file can be secured with gpg or not).\n"
                  "\n"
                  "A single account configuration file '" *MAIL-PACK-CREDENTIALS-FILE* "' would look like this:\n"
                  "machine email-description firstname <firstname> surname <surname> name <name> x-url <url> mail <email> mail-host <mail-host> signature <signature>\n"
                  "machine imap.gmail.com login <your-email> password <your-mail-password-or-dedicated-passwd> port 993\n"
                  "machine smtp.gmail.com login <your-email> port 587 password <your-mail-password-or-dedicated-passwd>\n"
                  "\n"
                  "A multiple account configuration file '" *MAIL-PACK-CREDENTIALS-FILE* "' would look like this:\n"
                  "machine email-description firstname <firstname> surname <surname> name <name> x-url <url> mail <email> mail-host <mail-host> signature <signature>\n"
                  "machine imap.gmail.com login <your-email> password <your-mail-password-or-dedicated-passwd> port 993\n"
                  "machine smtp.gmail.com login <login> port 587 password <your-mail-password-or-dedicated-passwd>\n"
                  "machine 2-email-description firstname <firstname> surname <surname> name <name> x-url <url> mail <email> mail-host <mail-host> signature <signature>\n"
                  "machine 2-imap.gmail.com login <2nd-email> password <your-mail-password-or-dedicated-passwd> port 993\n"
                  "machine 2-smtp.gmail.com login <2nd-email> port 587 password <your-mail-password-or-dedicated-passwd>\n"
                  "machine 3-email-description firstname <firstname> surname <surname> name <name> x-url <url> mail <email> mail-host <mail-host> signature <signature>\n"
                  "...\n"
                  "\n"
                  "Optional: Then `M-x encrypt-epa-file` to generate the required ~/.authinfo.gpg and remove ~/.authinfo.")))

(provide 'mail-pack)

;;; mail-pack.el ends here
