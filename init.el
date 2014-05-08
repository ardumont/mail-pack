;;; mail-pack.el --- mail-pack

;;; Commentary:

;;; Code:

(install-packs '(s
                 dash
                 creds
                 google-contacts
                 offlineimap))

;; emacs internal libs
(require 'gnus)
(require 'epa-file)
(require 'smtpmail)

;; external libs - installed from marmalade/melpa
(require 'creds)
(require 'dash)
(require 's)
(require 'google-contacts)
(require 'google-contacts-message)
(require 'offlineimap)

;; external libs (installed from package manager)

;; install mu in your system `sudo aptitude install -y mu` and update the path on your machine to mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)

;; ===================== Setup

;; activate option to keep the passphrase (it's preferable to use gpg-agent)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; create your .authinfo file and and encrypt it in ~/.authinfo.gpg with M-x epa-encrypt-file
(defvar *MAIL-PACK-MAIL-ROOT-FOLDER* (expand-file-name "~/.mails")
  "The root folder where you store your maildirs.")

(defvar *MAIL-PACK-CREDENTIALS-FILE* (expand-file-name "~/.authinfo.gpg")
  "The credentials file where you store your email informations. This can be plain text too.")

(defvar *MAIL-PACK-PERIOD-FETCH-MAIL* 600
  "Number of seconds between fetch + indexing. Default to 600 seconds.")

(defvar *MAIL-PACK-INTERACTIVE-CHOOSE-ACCOUNT* t
  "Let the user decide if (s)he wants to choose the account to use when composing.
If set to t, the main account will be automatically be chosen (to change the main account, use M-x mail-pack/set-main-account!
Otherwise, each time the user will compose an email, it will be asked to choose the account to use.
By default t.")

;; ===================== functions

(defun mail-pack/log (str) "A log function for the pack."
  (message "Mail Pack - %s" str))

(defun mail-pack/setup-possible-p (creds-file)
  "Check if the setup is possible by checking the existence of the file creds-file
and that the entries 'imap.gmail.com', 'smtp.gmail.com', and 'email-description' are provided.
If all is ok, return the creds-file's content, nil otherwise."
  (when (file-exists-p creds-file)
    (let ((creds-file-content (creds/read-lines creds-file)))
      (when (and (creds/get creds-file-content "email-description")
                 (creds/get creds-file-content "smtp.gmail.com"))
        creds-file-content))))

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
 If all accounts are found, return the first one." ;; TODO look at mu4e-message-contact-field-matches -> (mu4e-message-contact-field-matches msg :to "me@work.com"))
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
  "Given the ACCOUNTS list, return only the list of possible maildirs."
  (mapcar #'car accounts))

(defun mail-pack/choose-main-account! (possible-accounts)
  "Permit the user to choose an account from the optional ACCOUNT-LIST as main account. Return the chosen account."
  (completing-read (format "Compose with account: (%s) " (s-join "/" possible-accounts))
                   possible-accounts nil t nil nil (car possible-accounts)))

(defun mail-pack/set-main-account! ()
  (interactive)
  (let* ((accounts          *MAIL-PACK-ACCOUNTS*)
         (possible-accounts (mail-pack/--maildir-accounts accounts))
         (account           (mail-pack/choose-main-account! possible-accounts)))
    (-> account
      (assoc accounts)
      mail-pack/--setup-as-main-account!)))

(defun mail-pack/set-account (accounts)
  "Set the account. When composing a new message, ask the user to choose. When replying/forwarding, determine automatically the account to use."
  (let* ((possible-accounts       (mail-pack/--maildir-accounts accounts))
         (composed-parent-message (mail-pack/--compute-composed-message!))
         ;; when replying/forwarding a message
         (retrieved-account (when composed-parent-message
                              (mail-pack/--retrieve-account composed-parent-message possible-accounts)))
         (account           (if retrieved-account
                                retrieved-account
                              ;; otherwise we need to choose (interactively or automatically) which account to choose
                              (if *MAIL-PACK-INTERACTIVE-CHOOSE-ACCOUNT*
                                  ;; or let the user choose which account he want to compose its mail
                                  (mail-pack/choose-main-account! possible-accounts)
                                (mail-pack/--maildir-from-email user-mail-address)))))
    (if account
        (-> account
          (assoc accounts)
          mail-pack/--setup-as-main-account!)
      (error "No email account found!"))))

(defun mail-pack/--setup-keybindings-and-hooks! ()
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
            (lambda () (mail-pack/set-account *MAIL-PACK-ACCOUNTS*))))

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
        mu4e-refile-folder "/[Gmail].All Mail"
        ;; setup some handy shortcuts
        mu4e-maildir-shortcuts `(("/INBOX"             . ?i)
                                 (,mu4e-sent-folder    . ?s)
                                 (,mu4e-trash-folder   . ?t)
                                 (,mu4e-drafts-folder  . ?d)
                                 (,mu4e-refile-folder  . ?a))
        ;; skip duplicates by default
        mu4e-headers-skip-duplicates t
        ;; default page size
        mu4e-headers-results-limit 500
        mu4e~headers-sort-direction 'descending
        mu4e~headers-sort-field :date
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
        mu4e-headers-fields '((:human-date    . 16)
                              (:flags         . 6)
                              (:from          . 25)
                              (:to            . 25)
                              ;; (:mailing-list  . 10)
                              (:size          . 10)
                              ;; (:tags          . 10)
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
        smtpmail-debug-info t
        smtpmail-debug-verb t
        ;; empty the hooks
        mu4e-headers-mode-hook nil
        mu4e-main-mode-hook nil
        mu4e-compose-pre-hook nil)
  ;; Add bookmarks query
  (add-to-list 'mu4e-bookmarks '("size:5M..500M" "Big messages" ?b) t)
  (add-to-list 'mu4e-bookmarks '("date:today..now AND flag:unread AND NOT flag:trashed" "Unread messages from today" ?U)))

(defun mail-pack/--compute-fullname (firstname surname name)
  "Given the user's FIRSTNAME, SURNAME and NAME, compute the user's fullname."
  (cl-flet ((if-null-then-empty (v) (if v v "")))
    (s-trim (format "%s %s %s" (if-null-then-empty firstname) (if-null-then-empty surname) (if-null-then-empty name)))))

(defun mail-pack/--maildir-from-email (mail-address)
  "Compute the maildir (without its root folder) from the MAIL-ADDRESS."
  (car (s-split "@" mail-address)))

(defun mail-pack/--setup-as-main-account! (account-setup-vars)
  "Given the entry ACCOUNT-SETUP-VARS, set the main account vars up."
  (mapc #'(lambda (var) (set (car var) (cadr var))) (cdr account-setup-vars)))

(defun mail-pack/--setup-account (creds-file creds-file-content &optional entry-number)
  "Given the CREDS-FILE path, the CREDS-FILE-CONTENT and an optional ENTRY-NUMBER, setup one account.
If ENTRY-NUMBER is not specified, we are dealing with the main account. Other it's a secondary account."
  (let* ((description-entry        (creds/get creds-file-content (mail-pack/--label entry-number "email-description")))
         (full-name                (mail-pack/--compute-fullname (creds/get-entry description-entry "firstname")
                                                                 (creds/get-entry description-entry "surname")
                                                                 (creds/get-entry description-entry "name")))
         (x-url                    (creds/get-entry description-entry "x-url"))
         (mail-host                (creds/get-entry description-entry "mail-host"))
         (signature                (creds/get-entry description-entry "signature-file"))
         (smtp-server              (creds/get-entry description-entry "smtp-server"))
         (mail-address             (creds/get-entry description-entry "mail"))
         (smtp-server-entry        (creds/get-with creds-file-content `(("machine" . ,smtp-server) ("login" . ,mail-address))))
         (smtp-port                (creds/get-entry smtp-server-entry "port"))
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
                                     (smtpmail-smtp-user ,mail-address)
                                     (smtpmail-starttls-credentials ((,smtp-server ,smtp-port nil nil)))
                                     (smtpmail-smtp-service         ,smtp-port)
                                     (smtpmail-default-smtp-server  ,smtp-server)
                                     (smtpmail-smtp-server          ,smtp-server)
                                     (smtpmail-auth-credentials     ,creds-file)
                                     ;; mu4e setup
                                     (mu4e-maildir ,(expand-file-name folder-root-mail-address)))))
    ;; Sets the main account if it is the one!
    (unless entry-number
      (mail-pack/--setup-as-main-account! account-setup-vars))
    ;; In any case, return the account setup vars
    account-setup-vars))

(defvar *MAIL-PACK-ACCOUNTS* nil "User's email Accounts.")

(defun mail-pack/setup (creds-file creds-file-content)
  "Mail pack setup with the CREDS-FILE path and the CREDS-FILE-CONTENT."
  ;; common setup
  (mail-pack/--common-configuration!)

  ;; reinit the accounts list
  (setq *MAIL-PACK-ACCOUNTS*)

  ;; secondary accounts setup
  (-when-let (nb-accounts (mail-pack/--nb-accounts creds-file-content))
    (when (< 1 nb-accounts)
      (->> (number-sequence 2 nb-accounts)
        (--map (mail-pack/--setup-account creds-file creds-file-content (format "%s" it)))
        (--map (add-to-list '*MAIL-PACK-ACCOUNTS* it)))))

  ;; main account setup
  (add-to-list '*MAIL-PACK-ACCOUNTS* (mail-pack/--setup-account creds-file creds-file-content))

  ;; install bindings and hooks
  (mail-pack/--setup-keybindings-and-hooks!))

;; ===================== Starting the mode

(-if-let (creds-file-content (mail-pack/setup-possible-p *MAIL-PACK-CREDENTIALS-FILE*))
    (progn
      (mail-pack/log (concat *MAIL-PACK-CREDENTIALS-FILE* " found! Running Setup..."))
      (mail-pack/setup *MAIL-PACK-CREDENTIALS-FILE* creds-file-content)
      (mail-pack/log "Setup done!"))
  (mail-pack/log
   (concat
    "You need to setup your credentials file " *MAIL-PACK-CREDENTIALS-FILE* " for this to work. (The credentials file can be secured with gpg or not).\n"
    "\n"
    "A single account configuration file '" *MAIL-PACK-CREDENTIALS-FILE* "' would look like this:\n"
    "machine email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n"
    "machine smtp.gmail.com login <your-email> port 587 password <your-mail-password-or-dedicated-passwd>\n"
    "\n"
    "A multiple account configuration file '" *MAIL-PACK-CREDENTIALS-FILE* "' would look like this:\n"
    "machine email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n\n"
    "machine smtp.gmail.com login <login> port 587 password <your-mail-password-or-dedicated-passwd>\n"
    "machine 2-email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n\n"
    "machine smtp.gmail.com login <2nd-email> port 587 password <your-mail-password-or-dedicated-passwd>\n"
    "machine 3-email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n\n"
    "...\n"
    "\n"
    "Optional: Then `M-x encrypt-epa-file` to generate the required ~/.authinfo.gpg and remove ~/.authinfo.\n"
    "Whatever you choose, reference the file you use in your emacs configuration:\n"
    "(setq *MAIL-PACK-CREDENTIALS-FILE* (expand-file-name \"~/.authinfo\"))")))

(provide 'mail-pack)

;;; mail-pack.el ends here
