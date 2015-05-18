# chatwork.el - ChatWork client for Emacs #

[![MELPA](http://melpa.org/packages/chatwork-badge.svg)](http://melpa.org/#/chatwork)

A client library of [ChatWork](http://www.chatwork.com/) for Emacs.

- ChatWork mode, one buffer for one room
  - Send a message to a room

## Install ##

### Use Package System ###

chatwork.el is available in MELPA.
Please use MELPA with package system.  It is easiest way to get the latest chatwork.el.

### From Github ###

You can get the latest chatwork.el from Github, too.

```
$ git clone https://github.com/ataka/chatwork.git
```

And put chatwork.el to your `load-path`.

### Get API Token ###

ChatWork API is in preview release.
You can get your ChatWork API Token from API page.

* [Get API Token](https://www.chatwork.com/service/packages/chatwork/subpackages/api/apply_beta.php)

You'll get an email to activate your API token.
Confirm it from web brower, and wait, wait and wait.
API token will be active in a few hours.

You can find your ChatWork API Token in API tab in Setting page in ChatWork.

### .emacs ###

Put the following code into your `.emacs.d/init.el`

```elisp
(setq chatwork-token "YOUR CHATWORK API TOKEN")
```

## chatwork-mode ##

Type `M-x chatwork`, then select your room.  A new buffer for chatwork room will open; which name is `*chatwork: YOUR ROOM NAME*`.

Write a message in that buffer, then type `C-cC-c`.  The message is sent to the room in ChatWork!

If you want to open other chatwork room, type `M-x chatwork` again.  `C-cC-b` will help to switch buffer for chatwork, if you have already opened it.

### Insert commands ###

Some Insert commands are prepared:

- `C-cC-iC-t` : Insert To tag
- `C-cC-iC-i` : Insert info tag
- `C-cC-iC-c` : Insert code tag
- `C-cC-iC-h` : Insert hr tag

If you type backquote three times, then three backquotes will be replaced with tag code.  It is easy way to insert code tag!

## Usage (not chatwork-mode) ##

Two interactive commands are available.

- `chatwork-send-message`
- `chatwork-send-message-in-region`

### `chatwork-send-message` ###

Input a message from mini-buffer, and select the room you want to send a message.

### `chatwork-send-message-in-region` ###

Send a message in a region, with selecting the room you want to send a message.

## TODO ##

- Create a task
  + List tasks
  + Update tasks
  + Delete tasks
  + Complete a task
- Show timeline
