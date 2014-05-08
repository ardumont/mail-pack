mail-pack
=========

Mail-pack is an emacs lib to setup one's client accounts email through a unique [~/.authinfo.gpg](http://www.emacswiki.org/emacs-en/GnusAuthinfo) (or simply plain *~/.authinfo*) file.

This is using mu4e - http://www.djcbsoftware.nl/code/mu/mu4e/index.html#Top, this will:
- synchronize your accounts through [offlineimap](http://docs.offlineimap.org/en/latest/index.html) (which needs to be setup too, see https://github.com/ardumont/dot-files/blob/master/.offlineimaprc for an example) in [Maildir](https://en.wikipedia.org/wiki/Maildir) format
- index local maildirs through [mu](http://www.djcbsoftware.nl/code/mu/)
- read the [Maildir](https://en.wikipedia.org/wiki/Maildir) format
- use [smtpmail](https://www.gnu.org/software/emacs/manual/html_mono/smtpmail.html) lib to send emails

# Pre-requisite

## Programs

Install [offlineimap](http://docs.offlineimap.org/en/latest/index.html) (sync emails account to maildir) + [mu](http://www.djcbsoftware.nl/code/mu/) (index maildirs) + [gnutls](http://gnutls.org/) (for secure protocol communication).

```sh
sudo aptitude install -y offlineimap mu gnutls
```

# Install

This is compatible with [emacs-live-packs](https://github.com/ardumont/emacs-live-packs) and [prelude-packs](https://github.com/ardumont/prelude-packs).

## [emacs-live-packs](https://github.com/ardumont/emacs-live-packs)

Add this snippet in your `.emacs-live.el`:
```lisp
(emacs-live-packs/add-live-packs "~/.emacs-live-packs/" '("mail-pack"))
```

## [prelude-packs](https://github.com/ardumont/prelude-packs)

Add this snippet in your `prelude-packs.el`:
```lisp
(prelude-packs/load-packs "~/.prelude-packs/" '("mail-pack"))
```

## Standard emacs

You can of course simply install from emacs.
But you'll need this small template of code to https://github.com/ardumont/install-packages-pack/blob/master/init.el.

# Conventions

Some established conventions:
- the root maildir is expected by default to be *~/.mails*

- the maildir for an account is named after the login part of the email.
For example, mailpack expects *someone@somewhere.com* to be stored on disk at *~/.mails/someone*.

- the credentials file is in *~/.authinfo.gpg*

- every 600 seconds, mu4e will trigger offlineimap to sync the maildir and the imap accounts + mu to index the emails in maildir

- by default, on multi accounts setup, each time you compose an email, you will be asked to choose which account you want to use.

This can be changed through those variables:

```lisp
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

Simply change the variable to the values you wish:

```lisp
(setq variable value)
```

Then execute the code <kbd>M-x eval-last-sexp</kbd>.

Here is a sample:

```lisp
(setq *MAIL-PACK-MAIL-ROOT-FOLDER* (expand-file-name "~/Maildir"))
(setq *MAIL-PACK-CREDENTIALS-FILE* (expand-file-name "~/.authinfo"))
(setq *MAIL-PACK-PERIOD-FETCH-MAIL* 3600)
(setq *MAIL-PACK-INTERACTIVE-CHOOSE-ACCOUNT* nil)
```

Then reload the mail-pack.

## Load/reload

If you are in need of reloading the mail-pack because of some change in your setup (you added or removed an account in your ~/.authinfo.gpg for example), use:

<kbd>M-x mail-pack/load-mail-pack!</kbd>

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

## Multiple accounts

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

# Run

To start using mu4e: `M-x mu4e` or `C-c m e`

Convention to read keybindings:

Keybindings        | Action
-------------------|----------------------------------------------------------------------------------------------------------------------------------
<kbd>a</kbd>       | Hit *a*
<kbd>a/A</kbd>     | Hit *a* or *A*
<kbd>b i</kbd>     | Hit *b* followed by *i*
<kbd>C-c</kbd>     | Maintain *CTRL* and hit *c*

## mu4e-main

This is the main menu when you start mu4e.

Keybindings        | Description
-------------------|----------------------------------------------------------------------------------------------------------------------------------
<kbd>U</kbd>       | Launch offlineimap + mu for indexing
<kbd>c/C</kbd>     | Compose an email (in interactive mode, will ask for your account if multiple)
<kbd>j</kbd>       | Access to maildir folders
<kbd>j i</kbd>     | Jump to inbox folder
<kbd>j s</kbd>     | Jump to sent emails folder
<kbd>j o</kbd>     | Jump to other emails folder
<kbd>s</kbd>       | Search through your email through specific *mu* request, some examples: http://www.djcbsoftware.nl/code/mu/mu4e/Queries.html#Queries
<kbd>b</kbd>       | Access to bookmarks (pre-recorded searches)
<kbd>b u</kbd>     | Access to all unread messages
<kbd>b U</kbd>     | Access to all unread messages from today
<kbd>b t</kbd>     | Access to today's messages
<kbd>b t</kbd>     | Access to last seven days messages
<kbd>b p</kbd>     | Access to messages with images
<kbd>b B</kbd>     | Access to messages with big attachments (more than 5Mb)
<kbd>q</kbd>       | Quit

## mu4e-headers

This is the menu you see when jumping to a specific maildir folder or using bookmarks.
This presents an email lists.
The buffer is in a dired similar mode. You can mark emails and execute those marks with `x`.

Keybindings        | Description
-------------------|----------------------------------------------------------------------------------------------------------------------------------
<kbd>R/r</kbd>     | Compose reply at point
<kbd>f</kbd>       | Forward email at point
<kbd>n</kbd>       | Next email in list
<kbd>p</kbd>       | Previous email in list
<kbd>Enter/o</kbd> | Read email at point
<kbd>Space</kbd>   | When reading an email, scroll forward the email's content. When email finished, open the next one.
<kbd>?</kbd>       | Mark as unread
<kbd>a</kbd>       | Archive the email
<kbd>d</kbd>       | Move to trash
<kbd>D</kbd>       | Delete
<kbd>u</kbd>       | Remove mark at point
<kbd>U</kbd>       | Remove all marks
<kbd>x</kbd>       | Execute
<kbd>q<kbd>        | Quit

## Sending email

When composing an email, you have some convenients bindings too.

Keybindings        | Description
-------------------|----------------------------------------------------------------------------------------------------------------------------------
<kbd>C-c C-c</kbd> | Send the email
<kbd>C-c C-b</kbd> | Move directly to the body of the message
<kbd>C-c C-c</kbd> | Send the message and close the buffer
<kbd>C-c C-d</kbd> | Do not send the message but save as draft and bury the buffer
<kbd>C-c M-f</kbd> | insert file's content at point
<kbd>C-c C-a</kbd> | Insert file as attachment
<kbd>C-c C-k</kbd> | Do not save the message as draft and close the buffer
<kbd>C-c TAB</kbd> | Move to the signature of the message
<kbd>C-c ?</kbd>   | Display the bindings for more
