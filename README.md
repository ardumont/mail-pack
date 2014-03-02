mail-pack
=========

Pack to setup one's client mail for gmail.
This is using gnus - http://www.emacswiki.org/cgi-bin/wiki/GnusGmail.

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

Create a `~/.authinfo` file with your email credentials.

``` txt
machine imap.gmail.com login your-email@gmail.com password your-password-generated-for-emacs-in-google-account port 993
machine smtp.gmail.com login your-email port 587 password your-password-generated-for-emacs-in-google-account
machine description firstname Login surname Surname name Name x-url some-url-you-like mail Email mail-host Hostname-of-your-server
```

Do not change the first entries `machine imap.gmail.com`, `machine smtp.gmail.com` and `machine description`, they are static and used by this pack.

Example:

``` txt
machine imap.gmail.com login tony@gmail.com password your-password-generated-for-emacs-in-google-account port 993
machine smtp.gmail.com login tony port 587 password your-password-generated-for-emacs-in-google-account
machine description tony Login romain Surname dumont x-url http://adumont.fr/blog/ mail tony@gmail.com mail-host arrakis
```

# run

`M-x gnus`

# Important note

- **You need to do a clean exit of `gnus`!**
- If you don't, it will do an "initial download" of your emails at each new session (approx 10 megabytes for me).
- But with a "clean exit" only the delta (a few kilobytes) will be downloaded between the current and previous session.
- "Clean exit": `q` by default (`M-x gnus-group-exit`)
