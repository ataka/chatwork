# chatwork.el - ChatWork client for Emacs #

A client library of [ChatWork](http://www.chatwork.com/) for Emacs.

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

## Usage ##

Now two interactive commands are available.

- `chatwork-send-message`
- `chatwork-send-message-in-region`

### `chatwork-send-message` ###

Input a message from mini-buffer, and select the room you want to send a message.

### `chatwork-send-message-in-region` ###

Send a message in a region, with selecting the room you want to send a message.

## TODO ##

- Message draft buffer
  + Completion to input ChatWork tags, such as To and info
- Create a task
  + List tasks
  + Update tasks
  + Delete tasks
  + Complete a task
- Show timeline
