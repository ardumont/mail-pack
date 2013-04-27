mail-pack
=========

Pack to setup one's client mail for gmail.
This is using gnus - http://www.emacswiki.org/cgi-bin/wiki/GnusGmail.

# install

In your `.emacs-live.el` add this snippet:
```elisp
(live-add-packs '(mail-pack))
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

- `gnus` will download a lot of stuff (approx 10 megabytes for me) the first time you use it with your user account
- **If you don't quit properly** it will download everything at each session!
- By quitting properly it will download only the "delta" between the previous session and the new one (a few kilobytes)
- Quitting properly: `q` by default (`M-x gnus-group-exit`)
