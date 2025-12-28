# REST Client Module

HTTP REST client integration for Emacs.

## Description

This module provides the [restclient](https://github.com/pashky/restclient.el) package, enabling you to write and execute HTTP requests directly from Emacs buffers. It's like Postman or curl, but integrated into your editor.

## Features

- Write HTTP requests in a simple, readable format
- Execute requests and view responses inline
- Variable substitution for reusable requests
- Support for all HTTP methods (GET, POST, PUT, DELETE, etc.)
- Request/response history
- JSON/XML formatting
- Authentication support

## Installation

Enable the module in your `init.el`:

```emacs-lisp
(doom! :offby1
       restclient)
```

Then run:

```shell
doom sync
```

## Usage

Create a file with `.http` or `.rest` extension (or use `M-x restclient-mode`):

```http
# Get user info
GET https://api.github.com/users/offbyone
User-Agent: Emacs Restclient

###

# Create a post
POST https://jsonplaceholder.typicode.com/posts
Content-Type: application/json

{
  "title": "Test Post",
  "body": "This is a test",
  "userId": 1
}
```

### Keybindings

| Key      | Command                | Description                    |
|----------|------------------------|--------------------------------|
| `C-c C-c` | `restclient-http-send-current` | Execute request at point |
| `C-c C-r` | `restclient-http-send-current-raw` | Execute and show raw response |
| `C-c C-v` | `restclient-http-send-current-stay-in-window` | Execute without switching windows |
| `C-c C-p` | `restclient-jump-prev` | Jump to previous request |
| `C-c C-n` | `restclient-jump-next` | Jump to next request |

### Variables

Use variables for reusable values:

```http
:host = api.example.com
:token = my-secret-token

GET https://:host/users
Authorization: Bearer :token
```

### Request Separators

Use `###` to separate multiple requests in one file.

## Tips

- Use `#` for comments
- Headers come after the request line, one per line
- Blank line separates headers from body
- Response appears in a new buffer
- Works great with `ob-restclient` for org-mode integration

## Related

For org-mode integration, consider adding `ob-restclient` package.
