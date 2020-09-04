Emacs RSS Major Mode
====================

This is an Emacs major modes for
- RSS Mode: shows a list of RSS entries that's a result of
  [offlinerss](https://github.com/emad-elsaid/offlinerss)
- RSS Entry Mode: Renders a RSS entry file

## Usage

Load `rss-mode.el` to your Emacs, and execute `rss-entries-list` function to
show a list of RSS files in your RSS inbox under `~/rss/INBOX` where
`offlinerss` saves them.

## Spacemacs integration

To open the `rss-enteries-list` in spacemacs with "Space S" add that to your
spacemacs configuration

```elisp
(spacemacs/set-leader-keys "S" 'rss-entries-list)
```
