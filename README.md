# miki

**miki** is a small wiki with small features :

* We do not track pages' changes.
* We do not store user informations on pages.
* Every user is admin -- because sharing is a question of confidence.
* Each page is formated using [Markdown](http://daringfireball.net/projects/markdown/) -- no more, no less.
* We do not support attachment, images, ... Except if you use them from external links.
* There is no database, each page is store in a file on the server.
* **But** we have a REST API \o/

## Installation

### Requirements 

* [erlang](http://www.erlang.org)

### Configuration

## APIs

### GET /users

List all users

_Response :_

```
200 (OK)
Content-Type: application/json

["greg", "muriel"]
```

### GET /users/{token}

Return the single user for a given token

_Parameters :_

* `token` :: _string_ - String token of a user.

_Response :_

```
200 (OK)
Content-Type: application/json

{ "ok": "greg" }
```

### POST /users

Create a new user

_Request :_

```
Content-Type: application/json

{ "username": Username, "password": Password, "token": MyToken }
```

_Response :_

```
200 (OK)
Content-Type: application/json

{ "ok": Username }
```

### DELETE /users/{username}/{token}

Delete user

_Parameters :_

* `username` :: _string_ - String name of the user to delete
* `token` :: _string_ - String token.

_Response :_

```
204 (No Content)
```

### PUT /users

#### Get token

_Request :_

```
Content-Type: application/json

{ "username": Username, "password": Password, "action": "new_token" }
```

_Response :_

```
200 (OK)
Content-Type: application/json

{ "ok": Token }
```

#### Change password

_Request :_

```
Content-Type: application/json

{ 
  "username": Username, "password": Password, 
  "new_password": NewPassword, "action": "new_password" 
}
```

_Response :_

```
200 (OK)
Content-Type: application/json

{ "ok": Username }
```

### GET /pages

Liste pages

_Response :_

```
200 (OK)
Content-Type: application/json

[ "index", "about" ]
```

### GET /pages/{page}

_Parameters :_

* `page` :: _string_ - String name of the page

_Response :_

```
200 (OK)
Content-Type: text/plain

# Hello mu

[index](#/page/index)

Lorem ipsum dolor sit amet, consectetur adipiscing elit. 
Mauris vitae lectus vel neque mattis sodales. 
```

### POST /pages

_Request :_

```
Content-Type: application/json

{ "title": Title, "content": Content, "token": Token }
```

_Response :_

```
200 (OK)
Content-Type: application/json

{ "ok": Title }
```

### DELETE /pages/{page}/{token}

Delete page

_Parameters :_

* `page` :: _string_ - String name of the page to delete
* `token` :: _string_ - String token.

_Response :_

```
204 (No Content)
```

## Licence

miki is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014 Grégoire Lejeune <<gregoire.lejeune@free.fr>>

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

