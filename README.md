mail-pack
=========

Pack to setup one's client email account through a unique *~/.authinfo.gpg* (or simply plain *~/.authinfo*) file.

This is using mu4e - http://www.djcbsoftware.nl/code/mu/mu4e/index.html#Top, this will:
- do the synchronization through offlineimap (which needs to be setup otherwise, see https://github.com/ardumont/dot-files/blob/master/.offlineimaprc for an example)
- index through mu
- read the Maildir format
- use gnus smtp to send emails

# pre-requisite

## mu

Install mu.

```sh
sudo aptitude install mu
```

## convention

Some convention established (but can be reedited):
- the root maildir is *~/.mails*
- the maildir for an account is named after the login part of the email.
For example, mailpack expects *someone@somewhere.com* to be stored on disk at *~/.mails/someone*.

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
- one for the description
- one for the credentials for the smtp part

```text
machine description mail YOUR-EMAIL smtp-server SMTP-SERVER firstname LOGIN surname SURNAME name NAME x-url SOME-URL-YOU-LIKE mail-host HOSTNAME-OF-YOUR-SERVER
machine SMTP-SERVER login YOUR-EMAIL port 587 password YOUR-PASSWORD-OR-YOUR-PASSWORD-GENERATED-FOR-EMACS-IN-GOOGLE-ACCOUNT
```

*Note* the `mail` and `smtp-server` entries form the foreign key used to find the credentials
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

When composing, mail-pack will ask you which account you want to use.

When replying/forwarding to an email, it will look the :to, :cc or :bcc from the current email and determine the account to use for composing/forwarding.

*Note*
For more than 2 accounts, add other entries by incrementing the number

# run

`M-x mu4e` or `C-c m e`
