mail-pack
=========

Pack to setup one's client mail for Gmail.
This is using mu4e - http://www.djcbsoftware.nl/code/mu/mu4e/index.html#Top, this will:
- read the Maildir format
- do no synchronization, this is supposed to be done by another process (I personally use offlineimap)
- use smtp to send emails

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

Create a `~/.authinfo` file with your email credentials on 2 separates machine lines:
- one for the description
- one for the credentials for the smtp part

``` txt
machine description mail your-email smtp-server Smtp-server firstname Login surname Surname name Name x-url some-url-you-like mail-host Hostname-of-your-server
machine smtp-server login your-email port 587 password your-password-or-your-password-generated-for-emacs-in-google-account
```

*Note* the `mail` and `smtp-server` entries are the keys used to find the next entry line.
This is important that those entries values map exactly to the values of the key `machine` and `login` associated.

Example:

``` txt
machine description mail tony@gmail.com smtp-server smtp.gmail.com firstname Login surname romain name dumont x-url http://adumont.fr/blog/ mail-host arrakis
machine smtp.gmail.com login tony@gmail.com port 587 password your-password-or-your-password-generated-for-emacs-in-google-account
```

## Multiple account

It's the same as for one account except that you need to prefix the `description` with the number of the account.

Here is a possible `~/.authinfo` for 2 gmail accounts:

```text
machine email-description mail some-email@gmail.com surname ardumont x-url some-url mail-host some-mail-host signature-file ~/.signature smtp-server smtp.gmail.com
machine smtp.gmail.com login some-email@gmail.com port 587 password some-pass
machine 2-email-description mail another-email@gmail.com firstname Antoine surname Romain name Dumont signature-file ~/.signature2 smtp-server smtp.gmail.com
machine smtp.gmail.com login another-email@gmail.com port 465 password another-pass
```

# run

`M-x mu4e` or `C-c m e`
