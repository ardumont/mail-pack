mail-pack
=========

Mail-pack is an emacs lib to setup one's client accounts email through a unique [~/.authinfo.gpg](http://www.emacswiki.org/emacs-en/GnusAuthinfo) (or simply plain *~/.authinfo*) file.

This is using mu4e - http://www.djcbsoftware.nl/code/mu/mu4e/index.html#Top, this will:
- synchronize your accounts through offlineimap (which needs to be setup otherwise, see https://github.com/ardumont/dot-files/blob/master/.offlineimaprc for an example) in [Maildir](https://en.wikipedia.org/wiki/Maildir) format
- index local maildirs through [mu](http://www.djcbsoftware.nl/code/mu/)
- read the [Maildir](https://en.wikipedia.org/wiki/Maildir) format
- use [smtpmail](https://www.gnu.org/software/emacs/manual/html_mono/smtpmail.html) lib to send emails

# pre-requisite

## programs

Install offlineimap + mu.

```sh
sudo aptitude install -y offlineimap mu
```

## convention

Some established conventions:
- the root maildir is expected by default to be *~/.mails*

- the maildir for an account is named after the login part of the email.
For example, mailpack expects *someone@somewhere.com* to be stored on disk at *~/.mails/someone*.

- the credentials file is in *~/.authinfo.gpg*

- every 600 seconds, mu4e will trigger offlineimap to sync the maildir and the imap accounts + mu to index the emails in maildir

- by default, on multi accounts setup, each time you compose an email, you will be asked to choose which account you want to use.

This can be changed through those variables:

``` elisp
(defvar *MAIL-PACK-MAIL-ROOT-FOLDER* (expand-file-name "~/.mails")
  "The root folder where you store your maildirs.")

(defvar *MAIL-PACK-CREDENTIALS-FILE* (expand-file-name "~/.authinfo.gpg")
  "The credentials file where you store your email informations. This can be plain text too.")

(defvar *MAIL-PACK-PERIOD-FETCH-MAIL* 600
  "Number of seconds between fetch + indexing. Default to 600 seconds.")

(defvar *MAIL-PACK-INTERACTIVE-CHOOSE-ACCOUNT* t
  "Let the user decide if (s)he wants to choose the account to use when composing.
If set to t (interactive mode), the main account will be automatically be chosen (to change the main account, use M-x mail-pack/set-main-account!
Otherwise (automatic mode), each time the user will compose an email, it will be asked to choose the account to use.
By default t.")
```

Simply change the value by:

```elisp
(setq variable value)
```

Then reload the mail-pack.

## load/reload

If you are in need of reloading the mail-pack (for example, change in the ~/.authinfo.gpg), use:

<kbd>M-x mail-pack/load-mail-pack!</kbd>


# Install

This is compatible with [emacs-live-packs](https://github.com/ardumont/emacs-live-packs) and [prelude-packs](https://github.com/ardumont/prelude-packs).

## [emacs-live-packs](https://github.com/ardumont/emacs-live-packs)

Add this snippet in your `.emacs-live.el`:
```elisp
(emacs-live-packs/add-live-packs "~/.emacs-live-packs/" '("mail-pack"))
```

## [prelude-packs](https://github.com/ardumont/prelude-packs)

Add this snippet in your `prelude-packs.el`:
```elisp
(prelude-packs/load-packs "~/.prelude-packs/" '("mail-pack"))
```

# Setup

## One account

Create a `~/.authinfo.gpg` file with your email credentials on 2 separates machine lines:
- one for the email description
- one for the email credentials for the smtp part

```text
machine description mail YOUR-EMAIL smtp-server SMTP-SERVER firstname LOGIN surname SURNAME name NAME x-url SOME-URL-YOU-LIKE mail-host HOSTNAME-OF-YOUR-SERVER
machine SMTP-SERVER login YOUR-EMAIL port 587 password YOUR-PASSWORD-OR-YOUR-PASSWORD-GENERATED-FOR-EMACS-IN-GOOGLE-ACCOUNT
```

*Note* the `mail` and `smtp-server` entries form a foreign key which will be used to find the credentials.
This is important that those entries values map exactly to the values of the key `machine` and `login` associated.

Example:

```text
machine description mail tony@gmail.com smtp-server smtp.gmail.com firstname Login surname romain name dumont x-url http://adumont.fr/blog/ mail-host arrakis
machine smtp.gmail.com login tony@gmail.com port 587 password your-password-or-your-password-generated-for-emacs-in-google-account
```

## Multiple account

It's the same as for one account except that you need to prefix the `description` with the number of the account.

Here is a possible `~/.authinfo.gpg` for 2 gmail accounts:

```text
machine email-description mail some-email@gmail.com surname ardumont x-url some-url mail-host some-mail-host signature-file ~/.signature smtp-server smtp.gmail.com
machine smtp.gmail.com login some-email@gmail.com port 587 password some-pass
machine 2-email-description mail another-email@gmail.com firstname Antoine surname Romain name Dumont signature-file ~/.signature2 smtp-server smtp.gmail.com
machine smtp.gmail.com login another-email@gmail.com port 465 password another-pass
```

*Notes*
- The first entry `email-description` is considered the default main account
- When composing, in interactive mode (**MAIL-PACK-INTERACTIVE-CHOOSE-ACCOUNT** set to t), mail-pack will ask you which account you want to use.
Otherwise, in automatic mode (**MAIL-PACK-INTERACTIVE-CHOOSE-ACCOUNT** set to nil), it will choose the main account to compose.
- When replying/forwarding to an email, it will look the :to, :cc or :bcc from the current email and determine the account to use for composing/forwarding.
If no email accounts is found, it will behave the same as when composing email.

*Remark*
This is not limited to 2 accounts, add other entries by incrementing the prefix number (3-email-description, 4-email-description, etc...).
Do not forget one account is set with 2 lines though.

## Set the main account

The main account is by default the one described by the entry `email-description`.
When in automatic mode, you will want to change your main account from time to time.
For this, use:
<kbd>M-x mail-pack/set-main-account!</kbd>

this will prompt you with the account to set as main.

# run

`M-x mu4e` or `C-c m e`
