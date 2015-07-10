;;; mail-pack.el --- A `pack` to setup your email accounts through a ~/.authinfo(.gpg) credentials file

;; Copyright (C) 2014 Antoine R. Dumont <eniotna.t AT gmail.com>

;; Author: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Maintainer: Antoine R. Dumont <eniotna.t AT gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (dash "2.11.0") (dash-functional "2.11.0") (creds "0.0.6.1") (offlineimap "0.1") (async "1.3"))
;; Keywords: emails offlineimap mu4e configuration
;; URL: https://github.com/ardumont/mail-pack

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A `pack` to setup your email accounts through a ~/.authinfo(.gpg) credentials file
;;
;; Enjoy!
;;
;; More informations on https://github.com/ardumont/mail-pack

;;; Code:


(defgroup mail-pack nil " Mail-pack group"
  :tag "Mail-pack"
  :version "0.0.1")

;; Internal libs
(require 'gnus)
(require 'epa-file)
(require 'smtpmail)

;; External libs - installed from marmalade/melpa
(require 'creds)
(require 'dash)
(require 's)
(require 'offlineimap)
(require 'smtpmail-async)

(require 'mail-pack-rules)

;; ===================== User setup (user can touch this, the preferred approach it to define a hook to override those values)

(defun mail-pack--compute-nix-mu4e-home ()
  "Compute mu4e home."
  (let ((mu-home (let ((coding-system-for-read 'utf-8))
                   (shell-command "echo $(dirname $(readlink $(which mu)))/..");; UGLY HACK - find the nix way to determine mu's home
                   (-> (with-current-buffer "*Shell Command Output*"
                         (buffer-string))
                       s-trim))))
    (format "%s%s" mu-home "/share/emacs/site-lisp/mu4e")))

;; Install mu in your system (deb-based: `sudo aptitude install -y mu`,
;; nix-based: `nix-env -i mu`) and update the path on your machine to mu4e
(defcustom mail-pack-mu4e-install-folder (if (file-exists-p "/etc/NIXOS")
                                             (mail-pack--compute-nix-mu4e-home)
                                           "/usr/share/emacs/site-lisp/mu4e")
  "The mu4e installation folder."
  :group 'mail-pack)

;; create your .authinfo file and and encrypt it in ~/.authinfo.gpg with M-x epa-encrypt-file
(defcustom mail-pack-mail-root-folder (expand-file-name "~/.mails")
  "The root folder where you store your maildirs folder."
  :group 'mail-pack)

(defcustom mail-pack-credentials-file (expand-file-name "~/.authinfo.gpg")
  "The credentials file where you store your email information.
This can be plain text too."
  :group 'mail-pack)

(defcustom mail-pack-period-fetch-mail 300
  "Number of seconds between fetch + indexing.
Default to 300 seconds."
  :group 'mail-pack)

(defcustom mail-pack-interactive-choose-account nil
  "Let the user decide which account to use for composing a message.
If set to nil (automatic), the main account will be automatically chosen.
To change the main account, use `M-x mail-pack/set-main-account!`.
Otherwise, interactive, the user will be asked to choose the account to use.
If only 1 account, this is the chosen account.
By default 'interactive."
  :group 'mail-pack)

;; ===================== Static setup (user must not touch this)

(defvar mail-pack-accounts nil "User's email accounts.")

(defvar mail-pack/setup-hooks nil "Use hooks for user to set their setup override.")
(setq mail-pack/setup-hooks) ;; reset hooks

;; ===================== functions

(defun mail-pack/log (str)
  "Log STR with specific pack prefix."
  (message "Mail Pack - %s" str))

(defun mail-pack/pre-requisites-ok-p! ()
  "Ensure that the needed installation pre-requisites are met.
Returns nil if problem."
  (when (file-exists-p mail-pack-mu4e-install-folder)
    (add-to-list 'load-path mail-pack-mu4e-install-folder)
    (require 'mu4e)))

(defun mail-pack/setup-possible-p (creds-file)
  "Check if CREDS-FILE exists and contain at least one account.
If all is ok, return the creds-file's content, nil otherwise."
  (when (file-exists-p creds-file)
    (let* ((creds-file-content (creds/read-lines creds-file))
           (email-description  (creds/get creds-file-content "email-description"))
           (account-server     (creds/get-entry email-description "smtp-server"))
           (account-email      (creds/get-entry email-description "mail")))
      (when (creds/get-with creds-file-content `(("machine" . ,account-server) ("login" . ,account-email)))
        creds-file-content))))

(defun mail-pack/--nb-accounts (creds-file-content)
  "In CREDS-FILE-CONTENT, compute how many accounts exist?"
  (--reduce-from (let ((machine (creds/get-entry it "machine")))
                   (if (and machine (string-match-p "email-description" machine))
                       (+ 1 acc)
                     acc))
                 0
                 creds-file-content))

(defun mail-pack/--find-account (emails-sent-to possible-account)
  "Determine the account to use in EMAILS-SENT-TO.
EMAILS-SENT-TO is the addresses in to, cc, bcc from the message received.
POSSIBLE-ACCOUNT is the actual accounts setup-ed."
  (--filter (string= possible-account (mail-pack/--maildir-from-email it)) emails-sent-to))

(defun mail-pack/--compute-composed-message! ()
  "Compute the composed message (Delegate this to mu4e)."
  mu4e-compose-parent-message)

(defun mail-pack/--retrieve-account (composed-parent-message possible-accounts)
  "Retrieve the mail account to which the COMPOSED-PARENT-MESSAGE was sent to.
This will look into the :to, :cc, :bcc fields to find the right account.
POSSIBLE-ACCOUNTS is the actual lists of accounts setup-ed.
If all accounts are found, return the first encountered." ;; TODO look at mu4e-message-contact-field-matches -> (mu4e-message-contact-field-matches msg :to "me@work.com"))
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
  (if (< 1 (length possible-accounts))
      (completing-read (format "Compose with account: (%s) " (s-join "/" possible-accounts))
                       possible-accounts nil t nil nil (car possible-accounts))
    (car possible-accounts)))

(defun mail-pack/set-main-account! ()
  "Switch the current account."
  (interactive)
  (let* ((accounts          mail-pack-accounts)
         (possible-accounts (mail-pack/--maildir-accounts accounts))
         (account           (mail-pack/choose-main-account! possible-accounts)))
    (-> account
      (assoc accounts)
      mail-pack/--setup-as-main-account!)))

(defun mail-pack/set-account (accounts)
  "Set the main account amongst ACCOUNTS.
When composing a message, in interactive mode, the user chooses the account.
When composing a message, in automatic mode, the main account is chosen.
When replying/forwarding, determine automatically the account to use.
If no account is found, revert to the composing message behavior."
  (let* ((possible-accounts       (mail-pack/--maildir-accounts accounts))
         (composed-parent-message (mail-pack/--compute-composed-message!))
         ;; when replying/forwarding a message
         (retrieved-account (when composed-parent-message
                              (mail-pack/--retrieve-account composed-parent-message possible-accounts)))
         (account           (if retrieved-account
                                retrieved-account
                              ;; otherwise we need to choose (interactively or automatically) which account to choose
                              (if mail-pack-interactive-choose-account
                                  ;; or let the user choose which account he want to compose its mail
                                  (mail-pack/choose-main-account! possible-accounts)
                                (mail-pack/--maildir-from-email user-mail-address)))))
    (if account
        (-> account
            (assoc accounts)
            mail-pack/--setup-as-main-account!)
      (error "No email account found!"))))

(defun mail-pack/--setup-keybindings-and-hooks! ()
  "Install defaults hooks and key bindings."
  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (define-key 'mu4e-main-mode-map (kbd "u") 'mu4e-update-mail-and-index)
              (define-key 'mu4e-main-mode-map (kbd "c") 'mu4e-compose-new)
              (define-key 'mu4e-main-mode-map (kbd "e") 'mu4e-compose-edit)
              (define-key 'mu4e-main-mode-map (kbd "f") 'mu4e-compose-forward)
              (define-key 'mu4e-main-mode-map (kbd "r") 'mu4e-compose-reply)))

  (add-hook 'mu4e-headers-mode-hook
            (lambda ()
              (define-key 'mu4e-headers-mode-map (kbd "o") 'mu4e-headers-view-message)
              (define-key 'mu4e-headers-mode-map (kbd "a") 'mu4e-headers-mark-for-refile)
              (define-key 'mu4e-headers-mode-map (kbd "c") 'mu4e-compose-new)
              (define-key 'mu4e-headers-mode-map (kbd "e") 'mu4e-compose-edit)
              (define-key 'mu4e-headers-mode-map (kbd "f") 'mu4e-compose-forward)
              (define-key 'mu4e-headers-mode-map (kbd "r") 'mu4e-compose-reply)))

  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (define-key 'mu4e-view-mode-map (kbd "A") 'mu4e-view-action)
              (define-key 'mu4e-view-mode-map (kbd "a") 'mu4e-headers-mark-for-refile)
              (define-key 'mu4e-view-mode-map (kbd "c") 'mu4e-compose-new)
              (define-key 'mu4e-view-mode-map (kbd "e") 'mu4e-compose-edit)
              (define-key 'mu4e-view-mode-map (kbd "f") 'mu4e-compose-forward)
              (define-key 'mu4e-view-mode-map (kbd "r") 'mu4e-compose-reply)))

  ;; Hook to determine which account to use before composing
  (add-hook 'mu4e-compose-pre-hook
            (lambda () (mail-pack/set-account mail-pack-accounts))))

(defun mail-pack/--label (entry-number label)
  "Given an ENTRY-NUMBER, and a LABEL, compute the full label."
  (if (or (null entry-number) (string= "" entry-number))
      label
    (format "%s-%s" entry-number label)))

(defun mail-pack/--common-configuration! ()
  "Install the common configuration between all accounts."
  (setq gnus-invalid-group-regexp "[:`'\"]\\|^$"
        ;; maildir prefix root
        mu4e-maildir mail-pack-mail-root-folder
        ;; skip duplicates by default
        mu4e-headers-skip-duplicates t
        ;; auto update headers if changes
        mu4e-headers-auto-update t
        ;; default page size
        mu4e-headers-results-limit 100
        ;; default sort ordering
        mu4e~headers-sort-direction 'descending
        ;; default field ordering
        mu4e~headers-sort-field :date
        ;; don't save message to Sent Messages, GMail/IMAP will take care of this
        mu4e-sent-messages-behavior 'delete
        ;; allow for updating mail using 'U' in the main view
        mu4e-get-mail-command "offlineimap"
        ;; update every 5 min
        mu4e-update-interval mail-pack-period-fetch-mail
        ;; inline images directly in the body message
        mu4e-view-show-images t
        ;; prefer plain text message
        mu4e-view-prefer-html nil
        ;; to convert html to org - prerequisite: `'sudo aptitude install -y pandoc`' or `'nix-env -i pandoc`'
        ;; mu4e-html2text-command "pandoc -f html -t org"
        mu4e-html2text-command "w3m -T text/html"
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
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        ;; autocomplete address - only consider email addresses that were seen
        ;; in personal messages (variable: mu4e-user-mail-address-list)
        mu4e-compose-complete-only-personal t
        ;; autocomplete address - addresses
        mu4e-compose-complete-addresses t
        message-kill-buffer-on-exit t
        ;; SMTP setup ; pre-requisite: gnutls-bin package installed
        message-send-mail-function 'async-smtpmail-send-it
        ;; no message signature as we use signature-file instead
        message-signature nil
        mu4e-compose-signature nil
        ;; auto-include signature
        mu4e-compose-signature-auto-include nil
        smtpmail-stream-type 'starttls
        starttls-use-gnutls t
        smtpmail-debug-info t
        smtpmail-debug-verb t
        ;; empty the hooks (permits to replay setup from scratch)
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

(defun mail-pack/refile-msg (default-folder)
  "Compute the refiling folder with default DEFAULT-FOLDER.
ARCHIVE-FOLDER is the catch-all folder."
  (lexical-let ((archive-folder default-folder))
    (lambda (msg)
      (mail-pack-rules-filter-msg msg archive-folder))))

(defun mail-pack/--setup-account (creds-file creds-file-content &optional entry-number)
  "Setup an account and return the key values structure.
CREDS-FILE represents the credentials file.
CREDS-FILE-CONTENT is the content of that same file.
ENTRY-NUMBER is the optional account number (multiple accounts setup possible).
When ENTRY-NUMBER is nil, the account to set up is considered the main account."
  (let* ((description-entry        (creds/get creds-file-content (mail-pack/--label entry-number "email-description")))
         (full-name                (mail-pack/--compute-fullname (creds/get-entry description-entry "firstname")
                                                                 (creds/get-entry description-entry "surname")
                                                                 (creds/get-entry description-entry "name")))
         (x-url                    (creds/get-entry description-entry "x-url"))
         (mail-host                (creds/get-entry description-entry "mail-host"))
         (signature                (creds/get-entry description-entry "signature-file"))
         (smtp-server              (creds/get-entry description-entry "smtp-server"))
         (mail-address             (creds/get-entry description-entry "mail"))
         (draft-folder             (creds/get-entry description-entry "draft-folder"))
         (sent-folder              (creds/get-entry description-entry "sent-folder"))
         (trash-folder             (creds/get-entry description-entry "trash-folder"))
         (archive-folder           (creds/get-entry description-entry "archive-folder"))
         (attachment-folder        (creds/get-entry description-entry "attachment-folder"))
         (smtp-server-entry        (creds/get-with creds-file-content `(("machine" . ,smtp-server) ("login" . ,mail-address))))
         (smtp-port                (creds/get-entry smtp-server-entry "port"))
         (folder-mail-address      (mail-pack/--maildir-from-email mail-address))
         (folder-root-mail-address (format "%s/%s" mail-pack-mail-root-folder folder-mail-address))
         (refile-archive-fn        (mail-pack/refile-msg archive-folder))
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
                                     (mu4e-drafts-folder  ,(concat "/" folder-mail-address draft-folder))
                                     (mu4e-sent-folder    ,(concat "/" folder-mail-address sent-folder))
                                     (mu4e-trash-folder   ,(concat "/" folder-mail-address trash-folder))
                                     (mu4e-attachment-dir ,attachment-folder)
                                     (mu4e-refile-folder  ,refile-archive-fn)
                                     ;; setup some handy shortcuts
                                     (mu4e-maildir-shortcuts ((,(concat "/" folder-mail-address "/INBOX")       . ?i)
                                                              (,(concat "/" folder-mail-address sent-folder)    . ?s)
                                                              (,(concat "/" folder-mail-address trash-folder)   . ?t)
                                                              (,(concat "/" folder-mail-address draft-folder)   . ?d)
                                                              (,(concat "/" folder-mail-address archive-folder) . ?a))))))
    ;; Sets the main account if it is the one!
    (unless entry-number
      (mail-pack/--setup-as-main-account! account-setup-vars))
    ;; In any case, return the account setup vars
    account-setup-vars))

;; spell check
;; (setq mu4e-compose-mode-hook)
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            "Settings for message composition."
            (require 'flyspell)
            (set-fill-column 72)
            (turn-on-flyspell)))

(defun mail-pack/setup (creds-file creds-file-content)
  "Mail pack setup with the CREDS-FILE path and the CREDS-FILE-CONTENT."
  ;; common setup
  (mail-pack/--common-configuration!)

  ;; reinit the accounts list
  (setq mail-pack-accounts)

  ;; secondary accounts setup
  (-when-let (nb-accounts (mail-pack/--nb-accounts creds-file-content))
    (when (< 1 nb-accounts)
      (->> (number-sequence 2 nb-accounts)
           (mapc (lambda (account-entry-number)
                   (->> account-entry-number
                        (format "%s")
                        (mail-pack/--setup-account creds-file creds-file-content)
                        (add-to-list 'mail-pack-accounts)))))))

  ;; main account setup
  (add-to-list 'mail-pack-accounts (mail-pack/--setup-account creds-file creds-file-content))

  (custom-set-variables '(mu4e-user-mail-address-list (mapcar (lambda (entry) (cadr (cadr entry))) mail-pack-accounts)))

  ;; install bindings and hooks
  (mail-pack/--setup-keybindings-and-hooks!))

;; ===================== Starting the mode

(defun mail-pack/load-pack! ()
  "Mail pack loading routine.
This will check if the pre-requisite are met.
If ok, then checks if an account file exists the minimum required (1 account).
If ok then do the actual loading.
Otherwise, will log an error message with what's wrong to help the user fix it."
  (interactive)
  ;; run user defined hooks
  (run-hooks 'mail-pack/setup-hooks)
  ;; at last the checks and load pack routine
  (if (mail-pack/pre-requisites-ok-p!)
      (-if-let (creds-file-content (mail-pack/setup-possible-p mail-pack-credentials-file))
          (progn
            (mail-pack/log (concat mail-pack-credentials-file " found! Running Setup..."))
            (mail-pack/setup mail-pack-credentials-file creds-file-content)
            (mail-pack/log "Setup done!"))
        (mail-pack/log
         (concat
          "You need to setup your credentials file " mail-pack-credentials-file " for this to work. (The credentials file can be secured with gpg or not).\n"
          "\n"
          "A single account configuration file '" mail-pack-credentials-file "' would look like this:\n"
          "machine email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n"
          "machine smtp.gmail.com login <your-email> port 587 password <your-mail-password-or-dedicated-passwd>\n"
          "\n"
          "A multiple account configuration file '" mail-pack-credentials-file "' would look like this:\n"
          "machine email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n\n"
          "machine smtp.gmail.com login <login> port 587 password <your-mail-password-or-dedicated-passwd>\n"
          "machine 2-email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n\n"
          "machine smtp.gmail.com login <2nd-email> port 587 password <your-mail-password-or-dedicated-passwd>\n"
          "machine 3-email-description firstname <firstname> surname <surname> name <name> x-url <url> mail-host <mail-host> signature <signature> smtp-server <smtp-server>\n\n"
          "...\n"
          "\n"
          "Optional: Then `M-x encrypt-epa-file` to generate the required ~/.authinfo.gpg and remove ~/.authinfo.\n"
          "Whatever you choose, reference the file you use in your emacs configuration:\n"
          "(setq mail-pack-credentials-file (expand-file-name \"~/.authinfo\"))")))
    (mail-pack/log "As a pre-requisite, you need to install the offlineimap and mu packages.
For example, on debian-based system, `sudo aptitude install -y offlineimap mu`...
When mu is installed, you also need to reference the mu4e (installed with mu) installation folder for this pack to work.")))

(defvar mail-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e l") 'mail-pack/load-pack!)
    (define-key map (kbd "C-c e m") 'mu4e)
    (define-key map (kbd "C-c e s") 'mail-pack/set-main-account!)
    (define-key map (kbd "C-c e u") 'mu4e-update-index)
    (define-key map (kbd "C-c e i") 'mu4e-interrupt-update-mail)
    map)
  "Keymap for mail-pack mode.")

(define-minor-mode mail-pack-mode
  "Minor mode to consolidate mail-pack extensions.

\\{mail-pack-mode-map}"
  :lighter " MP"
  :keymap mail-pack-mode-map)

(define-globalized-minor-mode global-mail-pack-mode mail-pack-mode mail-pack-on)

(defun mail-pack-on ()
  "Turn on `mail-pack-mode'."
  (mail-pack-mode +1))

(global-mail-pack-mode)

(provide 'mail-pack)
;;; mail-pack.el ends here
